
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


//////////////////////////////////////////
// Data structures

typedef struct {
	unsigned length; // number of chars used
	unsigned capacity; // number of POINTERS allocated
	char **data;
} Builder;

void write_data(Builder *builder, unsigned data_len, char *data) {
	unsigned i = 0;
	while (i < data_len) {
		unsigned x = builder->length / 1024, y = builder->length % 1024;
		if (y == 0) {
			if (x == builder->capacity) {
				unsigned new_capacity = builder->capacity ? 2 * builder->capacity : 1;
				char** new_data = (char**)malloc(new_capacity * sizeof (char*));
				memcpy(new_data, builder->data, builder->capacity * sizeof (char*));
				free(builder->data);
				builder->data = new_data;
				builder->capacity = new_capacity;
			}
			builder->data[x] = (char*)malloc(1024);
		}
		unsigned diff = min(1024 - y, data_len - i);
		memcpy(builder->data[x] + y, data + i, diff);
		i += diff;
	}
}

//////////////////////////////////////////
// Token List

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
	T_COMMA,
	T_SEMICOLON,
	T_COLON,
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
	{",", false},
	{";", false},
	{":", false},
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
	substr ident;
	Type type;
} Binding;

typedef enum {
	E_CONST_WORD,
} ExprVariant;

typedef enum {
	RET,
} StatementVariant;

typedef struct {
	substr name;
	int param_num;
	Binding *params;
	Type ret_type;
	Builder body;
} Proc;

typedef struct {
	int item_num;
	Proc *items;
} Items;

Type parse_type(TokenBranch **iter) {
	Type result;
	result.indirection = 0;
	while ((*iter)->variant == T_ASTERISK) {
		*iter += 1;
		result.indirection += 1;
	}
	result.referant = **iter;
	*iter += 1;
	return result;
}

Binding parse_binding(TokenBranch **iter) {
	Binding result;
	// this line would make more sense in the language we're implementing
	// iter: **[TokenBranch];
	// ident: *TokenBranch := iter[0];
	TokenBranch *ident = *iter;
	if (ident->variant != T_ALPHANUM) { // *ident.variant != T_ALPHANUM
		printf("Expected identifier in parameter list\n");
		exit(-1);
	}
	result.ident = ident->data.substr;

	*iter += 1;
	if ((*iter)->variant != T_COLON) { // *iter[0].variant != T_COLON
		printf("Expected colon in parameter type ascription\n");
		exit(-1);
	}

	*iter += 1;
	result.type = parse_type(iter);
	return result;
}

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
			{
				//this.params = cb->data.subtree;
				TokenTree subtree = cb->data.subtree;
				Binding *params = NULL;
				if (subtree.branch_num) {
					params = (Binding*)malloc(sizeof(Binding) * subtree.branch_num);
				}
				Binding *params_start = params;

				TokenBranch *pb = subtree.branches;
				TokenBranch *end = subtree.branches + subtree.branch_num;
				while (pb < end) {
					*params = parse_binding(&pb); // increases pb
					if (pb < end && pb->variant != T_COMMA) {
						printf("Expected comma in between parameters\n");
						exit(-1);
					}
					pb += 1;
					params += 1;
				}

				this.params = params;
				this.param_num = params - params_start;
			}

			cb+=2; // 3
			this.ret_type = parse_type(&cb); // increases cb; 3+x

			if (cb->variant != T_OPEN_BRACE) {
				printf("Expected brace enclosed algorithm in proc definition");
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

void destroy_ast(Items items) {
	printf("Leaked AST :)\n");
}

void test() {
	// @Bug put spaces in here and check tokenizer still works
	char *input = "main(argc:b64,argv:*[{b64,*[b8]}])->b64{return 0;}";
	int len = strlen(input);

	printf("Tokenizing...\n");
	TokenTree ts = tokenize_flat(input, len);

	{
#define NUM_TOKENS 27
		int variants[NUM_TOKENS] = {
			T_ALPHANUM, T_OPEN_PAREN,
				T_ALPHANUM, T_COLON, T_B64, T_COMMA,
				T_ALPHANUM, T_COLON, T_ASTERISK, T_OPEN_BRACK, T_OPEN_BRACE,
					T_B64, T_COMMA,
					T_ASTERISK, T_OPEN_BRACK, T_ALPHANUM, T_CLOSE_BRACK,
				T_CLOSE_BRACE, T_CLOSE_BRACK,
			T_CLOSE_PAREN, T_ARROW, T_B64,
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
		const int paren_expect = 8;
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

	{
		const int expected_items = 1;
		if (items.item_num != expected_items) {
			printf("Wrong number of procs: expected %d got %d\n",
					expected_items, items.item_num);
		}
		int param_num = items.items[0].param_num;
		int param_expect = 2;
		if (param_num != param_expect) {
			printf("Wrong param num: param_num == %d != %d\n",
					param_num, param_expect);
		}
		int brace_size = items.items[0].body.branch_num;
		int brace_expect = 3;
		if (brace_size != brace_expect) {
			printf("Wrong subtree size: body.num == %d != %d\n",
					brace_size, brace_expect);
		}
	}
	destroy_ast(items);
	destroy_tt(tt);  // @Cleanup will ast always borrow tt?
	                 // also what should I be @ing here...
	                 // @Clarity? @Design? @Architecture?
}

int main() {
	test();
}
