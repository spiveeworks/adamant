
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TOKEN_WIDTH 16

typedef struct {
	char str[TOKEN_WIDTH];
	bool is_keyword;
} TokenDef;

typedef enum {
	T_ALPHANUM = -1,
	T_OPEN_BRACE,
	T_OPEN_PAREN,
	T_OPEN_BRACK,
	T_CLOSE_BRACE,
	T_CLOSE_PAREN,
	T_CLOSE_BRACK,
	T_DEFINITION,
	T_SEMICOLON,
	T_ARROW,
	T_ASTERISK,
	T_RETURN,
	T_B64,

	// number of possible tokens, not to be used as a variant...
	UTOKEN_NUM,
} TokenVariant;

#define T_FIRST_OPEN T_OPEN_BRACE
#define T_LAST_OPEN T_OPEN_BRACK

#define T_TOTAL_OPENS (T_LAST_OPEN - T_FIRST_OPEN + 1)

// value TokenDef utokens[UTOKEN_NUM] = 
TokenDef utokens[UTOKEN_NUM] = {
	{"{", false},
	{"(", false},
	{"[", false},
	{"}", false},
	{")", false},
	{"]", false},
	{":=", false},
	{";", false},
	{"->", false},
	{"*", false},
	{"return", true},
	{"b64", true},
};

bool is_alphanum(char c) {
	return
		('0' <= c && c <= '9') ||
		('A' <= c && c <= 'Z') ||
		('a' <= c && c <= 'z') ||
		c == '_';
}

typedef struct {
	int len;
	char *start;
} substr;

struct TokenBranch;

typedef struct {
	int branch_num;
	struct TokenBranch *branches;
} TokenTree;

typedef struct TokenBranch {
	TokenVariant variant;
	union {
		substr substr;
		TokenTree subtree;
	} data;
} TokenBranch;

//////////////////////////////////////////
// Tokenizer

int prefix_whitespace(const char *input) {
	int result = 0;
	bool done = false;
	while (done) {
		char c = input[result];
		if (c == '\t' || c == '\n' || c == '\r' || c == ' ') {
			result++;
		} else {
			done = true;
		}
	}
	return result;
}

int prefix_token(const char *input, TokenDef *utoken) {
	char *str = utoken->str;
	bool is_keyword = utoken->is_keyword;
	int i = 0;
	while (input[i] != '\0' && str[i] != '\0') {
		if (input[i] == '\0' || input[i] != str[i]) {
			return 0;
		}
		i++;
	}
	if(is_keyword && is_alphanum(input[i])) {
		// keywords must be followed by whitespace or an operator
		// in order to tokenize
		return 0;
	}
	return i;
}

substr next_token(char *input) {
	substr result;
	// default to 1 so that tokenizer always moves forward
	// really we should report an error when we reach an unknown operator
	result.len = 1;
	result.start = input;
	bool valid = true;
	while (valid) {
		char c = input[result.len];
		valid = is_alphanum(c);
		if (valid) {
			result.len++;
		}
	}
	return result;
}

TokenTree new_tree(int max_branches) {
	TokenTree result;
	result.branch_num = 0;
	result.branches = NULL;

	if (max_branches > 0) {
		result.branches =
			(TokenBranch*) malloc(sizeof(TokenBranch) * max_branches);
		for (int i = 0; i < max_branches; i++) {
			// we don't actually need to initialize anything but num
			// we can set num later when we change variant to >0
			// we do this instead to satisfy linters
			TokenBranch* out = &result.branches[i];
			out->variant = -2;
			out->data.subtree.branch_num = 0;
			out->data.subtree.branches = NULL;
		}
	}

	return result;
}

// borrows utokens during function
// borrows input for lifetime of result
TokenTree tokenize_flat(char *input, int input_len) {
	char *end = input + input_len;

	TokenTree result = new_tree(input_len);

	while (input < end) {
		input = input + prefix_whitespace(input);
		TokenVariant variant = T_ALPHANUM;
		for (TokenVariant ti = 0; ti < UTOKEN_NUM; ti++) {
			int len = prefix_token(input, &utokens[ti]);
			if (len) {
				variant = ti;
				input += len;
				break;
			}
		}

		TokenBranch *branch = result.branches + result.branch_num;
		result.branch_num++;

		branch->variant = variant;
		if (variant == T_ALPHANUM) {
			substr substr = next_token(input);
			branch->data.substr = substr;
			input += substr.len;
		} else {
			// so that when we recursively destroy tt, we don't free garbage
			branch->data.subtree.branches = NULL;
		}
	}

	return result;
}

typedef struct {
	TokenTree tt;
	TokenBranch *remaining;
} groupResult;

groupResult group_tokens_starting_from(
	TokenBranch *remaining,
	TokenBranch *end,
	int close
) {
	TokenTree tt = new_tree(end - remaining);
	TokenBranch *out = tt.branches;
	int variant;
	while (remaining < end) {
		variant = remaining->variant;
		if (variant == close) {
			// so that we return one past the close bracket
			remaining++;
			break;
		}
		// TODO: detect bad close braces to prevent "unexpected: }"?
		*out = *remaining;
		// increment before checking open brackets, so that we can recurse
		remaining++;

		if (variant >= T_FIRST_OPEN && variant <= T_LAST_OPEN) {
			groupResult subtree = group_tokens_starting_from(remaining, end,
					variant + T_TOTAL_OPENS);
			out->data.subtree = subtree.tt;
			remaining = subtree.remaining;
		}
		tt.branch_num++;
		out++;
	}
	groupResult result;
	result.tt = tt;
	result.remaining = remaining;
	return result;
}

// reborrows strings from ts, but allocates new branches
TokenTree group_tokens(TokenTree ts) {
	TokenBranch *start = ts.branches;
	TokenBranch *end = start + ts.branch_num;
	groupResult tt = group_tokens_starting_from(start, end, -2);
	return tt.tt;
}

void destroy_tt(TokenTree tt) {
	for (int i = 0; i < tt.branch_num; i++) {
		TokenBranch *branch = &tt.branches[i];
		if (branch->variant != T_ALPHANUM) {
			destroy_tt(branch->data.subtree);
		}
	}
}

///////////////////////////
// parser

typedef enum {
	I_FUN_DEF
} ItemVariant;

typedef struct {
	int indirection;
	TokenBranch referant;
} Type;

typedef struct {
	substr name;
	TokenTree params;
	Type ret_type;
	TokenTree body;
} Proc;

typedef struct {
	int item_num;
	Proc *items;
} Items;

Items parse(TokenTree tt) {
	Items result;
	result.item_num = 0;
	result.items = (Proc*)malloc(sizeof (Proc) * (tt.branch_num / 3 + 1));

	TokenBranch *cb = tt.branches;
	TokenBranch *end = cb + tt.branch_num;

	while (cb < end) {
		Proc this;

		// f (...) -> ... {...}
		// 0 1     2      3+x
		if (cb + 3 < end && (cb + 2)->variant == T_ARROW) {
			this.name = cb->data.substr;
			if (cb->variant != T_ALPHANUM) {
				printf("Expected identifier after struct keyword");
				exit(-1);
			}
			this.name = cb->data.substr;

			cb++; //1
			if (cb->variant != T_OPEN_PAREN) {
				printf("Expected paren enclosed parameter list before proc definition");
				exit(-1);
			}
			this.params = cb->data.subtree;

			cb+=2; // 3
			this.ret_type.indirection = 0;
			while (cb->variant == T_ASTERISK) {
				cb++;
				this.ret_type.indirection++;
			}
			this.ret_type.referant = *cb;

			cb++; // 3+x
			if (cb->variant != T_OPEN_BRACE) {
				printf("Expected brace enclosed parameter list in proc definition");
				exit(-1);
			}
			this.body = cb->data.subtree;

			cb++; // 3+x+1
		} else {
			printf("Currently only proc definitions are supported.");
			exit(-1);
		}
		result.items[result.item_num] = this;
		result.item_num++;
	}

	return result;
}

/*
char *render(Items in) {
	char *result = (char*)malloc(100);
	Proc *current = in.items;
	Proc *end = current + in.item_num;
	while (current < end) {
		// is this what strncpy really returns?
		result = strncpy(result, current->name.start, current->name.len);
		*result++ = '(';
		render_token_tree(result, current->params);
		*result++ = ')';
		result = strncpy(result, " { ", 3);
		result = render_token_tree(result, current->body);
		result = strncpy(result, " } ", 3);
	}
	return result;
}
*/


void test() {
	// @Bug put spaces in here and check tokenizer still works
	char *input = "main()->b64{return 0;}";
	int len = strlen(input);

	printf("Tokenizing...\n");
	TokenTree ts = tokenize_flat(input, len);

	{
#define NUM_TOKENS 10
		int variants[NUM_TOKENS] = {
			T_ALPHANUM, T_OPEN_PAREN, T_CLOSE_PAREN, T_ARROW, T_B64,
			T_OPEN_BRACE, T_RETURN, T_ALPHANUM, T_SEMICOLON, T_CLOSE_BRACE
		};
		if (ts.branch_num != NUM_TOKENS) {
			printf("Wrong number of tokens: num == %d != %d\n",
					ts.branch_num, NUM_TOKENS);
		} else {
			for (int i = 0; i < NUM_TOKENS; i++) {
				int actual = ts.branches[i].variant;
				if (actual != variants[i]) {
					printf("Wrong variant: for %d expected %d got %d\n",
							i, variants[i], actual);
				}
			}
		}
	}

	printf("Grouping...\n");
	TokenTree tt = group_tokens(ts);
	destroy_tt(ts);

	{
		const int paren = 1;
		int paren_size = tt.branches[paren].data.subtree.branch_num;
		int paren_expect = 0;
		if (paren_size != paren_expect) {
			printf("Wrong subtree size: branches[%d].num == %d != %d\n",
					paren, paren_size, paren_expect);
		}
		const int brace = 4;
		int brace_size = tt.branches[brace].data.subtree.branch_num;
		int brace_expect = 3;
		if (brace_size != brace_expect) {
			printf("Wrong subtree size: branches[%d].num == %d != %d\n",
					brace, brace_size, brace_expect);
		}
	}

	printf("Parsing...\n");
	Items items = parse(tt);
	destroy_tt(tt);

	{
		const int expected_items = 1;
		if (items.item_num != expected_items) {
			printf("Wrong number of procs: expected %d got %d\n",
					expected_items, items.item_num);
		}
		const int brace = 4;
		int brace_size = items.items[0].body.branch_num;
		int brace_expect = 3;
		if (brace_size != brace_expect) {
			printf("Wrong subtree size: branches[%d].num == %d != %d\n",
					brace, brace_size, brace_expect);
		}
	}
}

int main() {
	test();
}
