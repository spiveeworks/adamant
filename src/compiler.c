
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
	T_SEMICOLON,
	T_PUB,
	T_OPEN,
	T_STRUCT,

	// number of possible tokens, not to be used as a variant...
	UTOKEN_NUM,
} TokenVariant;

#define T_FIRST_OPEN T_OPEN_BRACE
#define T_LAST_OPEN T_OPEN_BRACK

#define T_TOTAL_OPENS (T_LAST_OPEN - T_FIRST_OPEN)

// value TokenDef utokens[UTOKEN_NUM] = 
TokenDef utokens[UTOKEN_NUM] = {
	{"{", false},
	{"(", false},
	{"[", false},
	{"}", false},
	{")", false},
	{"]", false},
	{";", false},
	{"struct", true},
	{"pub", true},
	{"open", true},
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

TokenTree new_tree(int max_branches);

// borrows utokens during function
// borrows input for lifetime of result
TokenTree tokenize_flat(char *input, int input_len);

TokenTree group_tokens(TokenTree ts);

char *render_token_tree(char*out, TokenTree in);

void destroy_tt(TokenTree tt);

void test_tokenize();

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

TokenTree group_tokens(TokenTree ts) {
	TokenBranch *start = ts.branches;
	TokenBranch *end = start + ts.branch_num;
	groupResult tt = group_tokens_starting_from(start, end, -2);
	return tt.tt;
}

char *render_token_tree(char *out, TokenTree in) {
	for (int i = 0; i < in.branch_num; i++) {
		TokenBranch *branch = &in.branches[i];
		switch(branch->variant) {
			case T_ALPHANUM: ;
				substr substr = branch->data.substr;
				out = strncpy(out, substr.start, substr.len);
				break;
			case T_OPEN_BRACE:
				break;
			case T_OPEN_PAREN:
				break;
			case T_OPEN_BRACK:
				break;
			case T_CLOSE_BRACE:
				break;
			case T_CLOSE_PAREN:
				break;
			case T_CLOSE_BRACK:
				break;
			case T_SEMICOLON:
				break;
			case T_PUB:
				break;
			case T_OPEN:
				break;
			case T_STRUCT:
				break;
		}
	}
}

void destroy_tt(TokenTree tt) {
	for (int i = 0; i < tt.branch_num; i++) {
		TokenBranch *branch = &tt.branches[i];
		if (branch->variant != T_ALPHANUM) {
			destroy_tt(branch->data.subtree);
		}
	}
}

void test_tokenize() {
	char *input = "struct point{int x;int y;};";
	int len = strlen(input);
	printf("Tokenizing...\n");
	TokenTree ts = tokenize_flat(input, len);
	int variants[11] = { 7, -1, 0, -1, -1, 6, -1, -1, 6, 3, 6 };
	if (ts.branch_num != 11) {
		printf("Wrong number of tokens: num == %d != 11\n", ts.branch_num);
	} else {
		for (int i = 0; i < 11; i++) {
			int actual = ts.branches[i].variant;
			if (actual != variants[i]) {
				printf("Wrong variant: expected[%d] != %d\n", i, actual);
			}
		}
	}
	printf("Grouping...\n");
	TokenTree tt = group_tokens(ts);

	int actual = tt.branches[2].data.subtree.branch_num;
	if (actual != 8) {
		printf("Wrong subtree size: branches[2].num == %d != 6\n", actual);
	}

	destroy_tt(ts);
	destroy_tt(tt);
}

///////////////////////////
// parser

typedef enum {
	I_STRUCT_DECL,
	I_STRUCT_DEF,
	I_FUN_DEF,
	I_FUN_DECL
} ItemVariant;

typedef enum {
	PRIVATE,
	PUBLIC,
	OPEN,
} Public;

typedef struct {
	ItemVariant variant;
	Public public;
	substr name;
	TokenTree data;
} Item;

typedef struct {
	int item_num;
	Item *items;
} Module;

Module parse(TokenTree tt) {
	Module result;
	result.item_num = 0;
	result.items = (Item*)malloc(sizeof (Item) * (tt.branch_num / 3 + 1));

	TokenBranch *cb = tt.branches;
	TokenBranch *end = cb + tt.branch_num;

	while (cb < end) {
		Item this;
		if (cb->variant == T_PUB) {
			this.public = PUBLIC;
			cb++;
		} else if (cb->variant == T_OPEN) {
			this.public = OPEN;
			cb++;
		} else {
			this.public = PRIVATE;
		}

		if (cb->variant == T_STRUCT) {
			this.variant = I_STRUCT_DEF;
			cb++;
			if (cb->variant != T_ALPHANUM) {
				printf("Expected identifier after struct keyword");
				exit(-1);
			}
			this.name = cb->data.substr;
			cb++;
			if (cb->variant != T_OPEN_BRACE) {
				printf("Expected brace enclosed field list in struct definition");
				exit(-1);
			}
			this.data = cb->data.subtree;
		} else {
			printf("Currently only struct definitions are supported.");
			exit(-1);
		}
		result.items[result.item_num] = this;
		result.item_num++;
	}

	return result;
}

char *render(Module in) {
	char *result = (char*)malloc(100);
	Item *current = in.items;
	Item *end = current + in.item_num;
	while (current < end) {
		if (current->variant == I_STRUCT_DEF) {
			// is this what strncpy really returns?
			result = strncpy(result, "struct ", 6);
			result = strncpy(result, current->name.start, current->name.len);
			result = strncpy(result, " { ", 3);
			result = render_token_tree(result, current->data);
			result = strncpy(result, " } ", 3);
		}
	}
	return result;
}


int main() {
	test_tokenize();
}
