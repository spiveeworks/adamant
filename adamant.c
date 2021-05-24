#include <stdio.h>
#include <error.h>
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define ARRAY_SIZE(x) (sizeof (x) / sizeof *(x))

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;
typedef intptr_t sxx;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef uintptr_t uxx;

typedef struct {
    sxx size;
    char *data;
} str;

char *cstr(str str) {
    return strndup(str.data, str.size);
}

bool str_eq(str a, str b) {
    if (a.size != b.size) return false;
    return strncmp(a.data, b.data, a.size) == 0;
}

/*************/
/* Tokeniser */
/*************/

typedef enum {
    /* 0 - 255 represent their ascii equivalents */
    TOK_IDENT = 256,
    TOK_NUM,
    TOK_DEFINE,
    TOK_LOGIC_OR,
    TOK_LOGIC_AND,
    TOK_EQ,
    TOK_NEQ,
    TOK_LEQ,
    TOK_GEQ,
    TOK_LSHIFT,
    TOK_RSHIFT,
    TOK_VAR,
    TOK_FUNC
} token_id;

#define COMPOUND_OPERATOR_COUNT 9
const struct compound_operators {
    token_id id;
    char *word;
} compound_operators[COMPOUND_OPERATOR_COUNT] = {
    {TOK_DEFINE, ":="},
    {TOK_LOGIC_OR, "||"},
    {TOK_LOGIC_AND, "&&"},
    {TOK_EQ, "=="},
    {TOK_NEQ, "!="},
    {TOK_LEQ, "<="},
    {TOK_GEQ, ">="},
    {TOK_LSHIFT, "<<"},
    {TOK_RSHIFT, ">>"}
};

#define KEYWORD_COUNT 2
const struct keywords {
    token_id id;
    char *word;
} keywords[KEYWORD_COUNT] = {
    {TOK_VAR, "var"},
    {TOK_FUNC, "func"}
};

#define IS_LOWER(c) ('a' <= (c) && (c) <= 'z')
#define IS_UPPER(c) ('A' <= (c) && (c) <= 'Z')
#define IS_ALPHA(c) (IS_LOWER(c) || IS_UPPER(c))
#define IS_NUM(c) ('0' <= (c) && (c) <= '9')
#define IS_ALPHANUM(c) (IS_ALPHA(c) || IS_NUM(c))
#define IS_WHITESPACE(c) ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\r')
#define IS_PRINTABLE(c) (' ' <= (c) && (c) <= '~')

typedef struct token {
    token_id id;
    str substr;
    s64 val;
} *Token;

char *read_token(char *stream, Token token) {
    while (*stream != '\0' && IS_WHITESPACE(*stream)) {
        stream++;
    }
    token->val = 0;
    if (IS_ALPHA(*stream)) {
        token->id = TOK_IDENT;
        token->substr.data = stream;
        stream++;
        token->substr.size = 1;
        while (IS_ALPHANUM(*stream)) {
            token->substr.size++;
            stream++;
        }
        for (int i = 0; i < KEYWORD_COUNT; i++) {
            if (strlen(keywords[i].word) == token->substr.size
                && strncmp(keywords[i].word, token->substr.data, token->substr.size) == 0)
            {
                token->id = keywords[i].id;
                break;
            }
        }
    } else if (IS_NUM(*stream)) {
        token->id = TOK_NUM;
        token->substr.data = stream;
        token->substr.size = 1;
        token->val = *stream - '0';
        stream++;
        while (IS_NUM(*stream)) {
            token->substr.size++;
            token->val *= 10;
            token->val += *stream - '0';
            stream++;
        }
        if (IS_ALPHA(*stream)) {
            error(1, 0, "Letters are not allowed in numerals");
        }
    } else {
        token->id = (u8)*stream;
        token->substr.size = 1;
        token->substr.data = stream;
        for (s32 i = 0; i < COMPOUND_OPERATOR_COUNT; i++) {
            uxx size = strlen(compound_operators[i].word);
            if (strncmp(compound_operators[i].word, stream, size) == 0) {
                token->id = compound_operators[i].id;
                token->substr.size = size;
                break;
            }
        }
        stream += token->substr.size;
    }
    return stream;
}

/****************/
/* Instructions */
/****************/

typedef enum {
    OP_NULL,
    OP_MOV,
    OP_LOGIC_OR,
    OP_LOGIC_AND,
    OP_EQ,
    OP_NEQ,
    OP_LEQ,
    OP_GEQ,
    OP_LESS,
    OP_GREATER,
    OP_OR,
    OP_XOR,
    OP_ADD,
    OP_SUB,
    OP_AND,
    OP_LSHIFT,
    OP_RSHIFT,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_LOGIC_NOT,
    OP_NOT,
    OP_NEG,
    OP_MEMBER
} opcode;

typedef enum {
    ARG_NULL,
    ARG_VAL,
    ARG_DEREF,
    ARG_CONST,
} op_mode;

struct ref {
    op_mode mode;
    u64 id;
};

struct instruction {
    opcode opcode;
    struct ref target;
    struct ref arg1;
    struct ref arg2;
};

/***************/
/* Interpretor */
/***************/

#define STATIC_VAR_CAP 256
uxx static_var_count = 0;

struct static_var{
    str name;
    s64 val;
    bool is_const;
} static_vars[STATIC_VAR_CAP];

void execute_instruction(struct instruction *instruction) {
    u64 arg1 = 0;
    switch (instruction->arg1.mode) {
    case ARG_VAL:
        arg1 = static_vars[instruction->arg1.id].val;
        break;
    case ARG_DEREF:
        /* direct access to the compiler's memory! */
        arg1 = *(u64*)static_vars[instruction->arg1.id].val;
        break;
    case ARG_CONST:
        arg1 = instruction->arg1.id;
        break;
    case ARG_NULL:
        error(1, 0, "arg mode not set");
        break;
    }

    u64 arg2 = 0;
    switch (instruction->arg2.mode) {
    case ARG_VAL:
        arg2 = static_vars[instruction->arg2.id].val;
        break;
    case ARG_DEREF:
        /* direct access to the compiler's memory! */
        arg2 = *(u64*)static_vars[instruction->arg2.id].val;
        break;
    case ARG_CONST:
        arg2 = instruction->arg2.id;
        break;
    case ARG_NULL:
        error(1, 0, "arg mode not set");
        break;
    }

    u64 result = 0;
    switch (instruction->opcode) {
    case OP_NULL:
        return;
    case OP_MOV:
        result = arg1;
        break;
    case OP_LOGIC_OR:
        result = arg1 || arg2;
        break;
    case OP_LOGIC_AND:
        result = arg1 && arg2;
        break;
    case OP_EQ:
        result = arg1 == arg2;
        break;
    case OP_NEQ:
        result = arg1 != arg2;
        break;
    case OP_LEQ:
        result = arg1 <= arg2;
        break;
    case OP_GEQ:
        result = arg1 >= arg2;
        break;
    case OP_LESS:
        result = arg1 < arg2;
        break;
    case OP_GREATER:
        result = arg1 > arg2;
        break;
    case OP_OR:
        result = arg1 | arg2;
        break;
    case OP_XOR:
        result = arg1 ^ arg2;
        break;
    case OP_ADD:
        result = arg1 + arg2;
        break;
    case OP_SUB:
        result = arg1 - arg2;
        break;
    case OP_AND:
        result = arg1 & arg2;
        break;
    case OP_LSHIFT:
        result = arg1 << arg2;
        break;
    case OP_RSHIFT:
        result = arg1 >> arg2;
        break;
    case OP_MUL:
        result = arg1 * arg2;
        break;
    case OP_DIV:
        result = arg1 / arg2;
        break;
    case OP_MOD:
        result = arg1 % arg2;
        break;
    case OP_LOGIC_NOT:
        result = !arg1;
        break;
    case OP_NOT:
        result = ~arg1;
        break;
    case OP_NEG:
        result = -arg1;
        break;
    case OP_MEMBER:
        error(1, 0, "member operator not yet implemented");
    default:
        error(1, 0, "undefined/unimplemented opcode %d", instruction->opcode);
    }

    switch (instruction->target.mode) {
    case ARG_VAL:
        static_vars[instruction->target.id].val = result;
        break;
    case ARG_DEREF:
        *(u64*)static_vars[instruction->target.id].val = result;
        break;
    case ARG_CONST:
        error(1, 0, "instruction had const target mode");
        break;
    case ARG_NULL:
        error(1, 0, "arg mode not set");
        break;
    }
}

typedef enum {
    PRECEDENCE_GROUPING,
    PRECEDENCE_DISJUNCTIVE,
    PRECEDENCE_CONJUNCTIVE,
    PRECEDENCE_COMPARATIVE,
    PRECEDENCE_ADDITIVE,
    PRECEDENCE_MULTIPLICATIVE,
    PRECEDENCE_UNARY,
    PRECEDENCE_OFFSET
} precedence_level;

#define PRECEDENCE_COUNT 6

#define OPERATOR_COUNT 19
const struct {
    token_id tok;
    precedence_level precedence;
    opcode op;
} operators[OPERATOR_COUNT] = {
    {TOK_LOGIC_OR, PRECEDENCE_DISJUNCTIVE, OP_LOGIC_OR},
    {TOK_LOGIC_AND, PRECEDENCE_CONJUNCTIVE, OP_LOGIC_AND},
    {TOK_EQ, PRECEDENCE_COMPARATIVE, OP_EQ},
    {TOK_NEQ, PRECEDENCE_COMPARATIVE, OP_NEQ},
    {TOK_LEQ, PRECEDENCE_COMPARATIVE, OP_LEQ},
    {TOK_GEQ, PRECEDENCE_COMPARATIVE, OP_GEQ},
    {'<', PRECEDENCE_COMPARATIVE, OP_LESS},
    {'>', PRECEDENCE_COMPARATIVE, OP_GREATER},
    {'|', PRECEDENCE_ADDITIVE, OP_OR},
    {'^', PRECEDENCE_ADDITIVE, OP_XOR},
    {'+', PRECEDENCE_ADDITIVE, OP_ADD},
    {'-', PRECEDENCE_ADDITIVE, OP_SUB},
    {TOK_LSHIFT, PRECEDENCE_MULTIPLICATIVE, OP_LSHIFT},
    {TOK_RSHIFT, PRECEDENCE_MULTIPLICATIVE, OP_LSHIFT},
    {'&', PRECEDENCE_MULTIPLICATIVE, OP_AND},
    {'*', PRECEDENCE_MULTIPLICATIVE, OP_MUL},
    {'/', PRECEDENCE_MULTIPLICATIVE, OP_DIV},
    {'%', PRECEDENCE_MULTIPLICATIVE, OP_MOD},
    {'.', PRECEDENCE_OFFSET, OP_MEMBER},
};

struct partial_instruction{
    opcode op;
    struct ref arg;
    precedence_level precedence;
};

#define OP_STACK_CAP 16

struct op_stack {
    uxx prev_var_count;
    struct ref target;
    sxx lhs_count;
    struct partial_instruction lhs[OP_STACK_CAP];
    struct partial_instruction rhs;
};

void op_stack_pop(
    struct op_stack *stack,
    struct ref *rhs,
    struct instruction *instruction
) {
    stack->lhs_count--;

    uxx i = stack->lhs_count;
    instruction->opcode = stack->lhs[i].op;

    if (stack->lhs[i].arg.mode == ARG_NULL) {
        instruction->arg1 = *rhs;
        instruction->arg2.mode = ARG_CONST;
        instruction->arg2.id = 0;
    } else {
        instruction->arg1 = stack->lhs[i].arg;
        instruction->arg2 = *rhs;
    }

    if (rhs->mode == ARG_VAL
        && rhs->id >= stack->prev_var_count) static_var_count--;
    if (stack->lhs[i].arg.mode == ARG_VAL
        && stack->lhs[i].arg.id >= stack->prev_var_count) static_var_count--;

    instruction->target.mode = ARG_VAL;
    instruction->target.id = static_var_count;
    static_var_count++;

    *rhs = instruction->target;
}

bool op_stack_step(
    struct op_stack *stack,
    struct instruction *instruction
) {
    bool empty = stack->lhs_count == 0;
    bool rhs_noop = stack->rhs.op == OP_NULL;
    bool lhs_noop = empty || stack->lhs[stack->lhs_count - 1].op == OP_NULL;
    bool lhs_flush = !empty && stack->lhs[stack->lhs_count - 1].precedence
        >= stack->rhs.precedence;
    if (lhs_noop && rhs_noop && lhs_flush) {
        stack->lhs_count--;
        return false;
    } else if (!lhs_noop && lhs_flush) {
        /* emit an instruction and overwrite rhs->arg with the result */
        op_stack_pop(stack, &stack->rhs.arg, instruction);

        if (stack->lhs_count == 0 && stack->rhs.op == OP_NULL) {
            /* finished flushing stack, use our own target */
            static_var_count--;
            instruction->target = stack->target;
            stack->target = (struct ref){};
            stack->rhs = (struct partial_instruction){};
        }
        return true;
    } else if (stack->lhs_count == 0 && stack->rhs.op == OP_NULL) {
        /* no operations were added, just mov */
        instruction->opcode = OP_MOV;
        instruction->target = stack->target;
        instruction->arg1 = stack->rhs.arg;
        instruction->arg2.mode = ARG_CONST;
        instruction->arg2.id = 0;
        if (stack->rhs.arg.mode == ARG_VAL
            && stack->rhs.arg.id >= stack->prev_var_count) static_var_count--;
        stack->target.mode = 0;
        stack->target.id = 0;
        stack->rhs = (struct partial_instruction){};
        return true;
    } else {
        /* stack has been flushed past desired precedence level, push */
        if (stack->lhs_count >= OP_STACK_CAP) {
            error(1, 0, "too many nested expressions");
        }
        stack->lhs[stack->lhs_count] = stack->rhs;
        stack->lhs_count += 1;
        stack->rhs = (struct partial_instruction){};
        return false;
    }
}

void op_stack_push_unary(
    struct op_stack *stack,
    opcode op,
    precedence_level precedence
) {
    if (stack->lhs_count >= OP_STACK_CAP) {
        error(1, 0, "too many nested expressions");
    }
    if (stack->rhs.arg.mode != ARG_NULL) {
        error(1, 0, "pushed unary operator before RHS was resolved");
    }
    uxx i = stack->lhs_count;
    stack->lhs[i].arg.mode = ARG_NULL;
    stack->lhs[i].arg.id = 0;
    stack->lhs[i].op = op;
    stack->lhs[i].precedence = precedence;
    stack->lhs_count += 1;
}

void run(char *stream) {
    struct token token;
    struct instruction instruction;
    enum {
        MODE_INIT,
        MODE_STATEMENT,
        MODE_IDENT,
        MODE_EXPR,
        MODE_OPERATOR,
        MODE_EXPR_FLUSH_OP,
        MODE_EXPR_FLUSH_PAREN,
        MODE_EXPR_FLUSH_FINAL
    } mode = MODE_INIT;
    struct op_stack op_stack;
    str varname;
    bool is_var_decl;
    bool op_stack_result;

    memset(&op_stack, 0, sizeof(struct op_stack));

    while (true) {
        instruction.opcode = OP_NULL;

        switch (mode) {
        case MODE_INIT:
            if (op_stack.lhs_count != 0) {
                error(1, 0, "op stack was not empty at start of statement");
            }
            is_var_decl = false;
            mode = MODE_STATEMENT;
            break;
        case MODE_STATEMENT:
            stream = read_token(stream, &token);
            switch (token.id) {
            case '\0':
                return;
            case TOK_IDENT:
                varname = token.substr;
                mode = MODE_IDENT;
                break;
            case TOK_VAR:
                if (is_var_decl) {
                    error(1, 0, "repeated \"var\" keyword");
                }
                is_var_decl = true;
                break;
            default:
                error(1, 0, "currently only assignment/declaration statements are supported");
            }
            break;

        case MODE_IDENT:
            stream = read_token(stream, &token);
            if (token.id != TOK_DEFINE && token.id != '=') {
                error(1, 0, "expected '=' or ':=', got \"%s\"", cstr(token.substr));
            }
            if (token.id == TOK_DEFINE || is_var_decl) {
                static_vars[static_var_count].name = varname;
                static_vars[static_var_count].val = 0;
                static_vars[static_var_count].is_const = !is_var_decl;
                op_stack.target.id = static_var_count;
                op_stack.target.mode = ARG_VAL;
                static_var_count++;
            } else {
                for (sxx i = static_var_count - 1; i >= 0; i--) {
                    if (str_eq(varname, static_vars[i].name)) {
                        op_stack.target.id = i;
                        op_stack.target.mode = ARG_VAL;
                    }
                }
                if (op_stack.target.mode == ARG_NULL) {
                    error(1, 0, "undefined identifier '%s'", cstr(varname));
                }
            }
            op_stack.prev_var_count = static_var_count;
            mode = MODE_EXPR;
            break;

        case MODE_EXPR:
            if (op_stack.rhs.arg.mode != ARG_NULL) {
                error(1, 0, "parse state error");
            }
            stream = read_token(stream, &token);
            switch (token.id) {
            case TOK_NUM:
                op_stack.rhs.arg.mode = ARG_CONST;
                op_stack.rhs.arg.id = token.val;
                mode = MODE_OPERATOR;
                break;
            case TOK_IDENT:
                for (sxx i = static_var_count - 1; i >= 0; i--) {
                    if (str_eq(token.substr, static_vars[i].name)) {
                        op_stack.rhs.arg.id = i;
                        op_stack.rhs.arg.mode = ARG_VAL;
                    }
                }
                if (op_stack.rhs.arg.mode == ARG_NULL) {
                    error(1, 0, "undefined identifier '%s'", cstr(token.substr));
                }
                varname = token.substr;
                mode = MODE_OPERATOR;
                break;
            case '(':
                op_stack_push_unary(&op_stack, OP_NULL, PRECEDENCE_GROUPING);
                break;
            case '!':
                op_stack_push_unary(&op_stack, OP_LOGIC_NOT, PRECEDENCE_UNARY);
                break;
            case '-':
                /* TODO check and forbid strange expressions like x * -y */
                op_stack_push_unary(&op_stack, OP_NEG, PRECEDENCE_UNARY);
                break;
            case '~':
                op_stack_push_unary(&op_stack, OP_NOT, PRECEDENCE_UNARY);
                break;
            case '+':
                /* this no-op is nice for emphasising sign conventions */
                break;
            default:
                error(1, 0, "expected expression, got \"%s\"", cstr(token.substr));
                break;
            }
            break;

        case MODE_OPERATOR:
            stream = read_token(stream, &token);
            if (token.id == ')') {
                op_stack.rhs.op = OP_NULL;
                op_stack.rhs.precedence = PRECEDENCE_GROUPING;
                mode = MODE_EXPR_FLUSH_PAREN;
            } else if (token.id == ';') {
                mode = MODE_EXPR_FLUSH_FINAL;
            } else {
                bool found = false;
                for (uxx i = 0; i < OPERATOR_COUNT; i++) {
                    if (token.id != operators[i].tok) continue;

                    found = true;
                    op_stack.rhs.op = operators[i].op;
                    op_stack.rhs.precedence = operators[i].precedence;
                    mode = MODE_EXPR_FLUSH_OP;
                    break;
                }
                if (!found) {
                    error(1, 0, "expected binary operator or ';', got \"%s\"", cstr(token.substr));
                }
            }
            break;
        case MODE_EXPR_FLUSH_OP:
            op_stack_step(&op_stack, &instruction);
            if (op_stack.rhs.arg.mode == ARG_NULL) {
                mode = MODE_EXPR;
            }
            break;
        case MODE_EXPR_FLUSH_PAREN:
            op_stack_result = op_stack_step(&op_stack, &instruction);
            if (!op_stack_result) {
                mode = MODE_OPERATOR;
            }
            break;
        case MODE_EXPR_FLUSH_FINAL:
            op_stack_step(&op_stack, &instruction);
            if (op_stack.rhs.arg.mode == ARG_NULL) {
                mode = MODE_INIT;
            }
            break;
        default:
            error(1, 0, "parse state corrupted");
            break;
        }

        if (instruction.opcode != OP_NULL) execute_instruction(&instruction);
    }
}

/****************/
/* Input/Output */
/****************/

str read_file(char *path) {
    FILE *input = NULL;
    str contents;

    input = fopen(path, "r");
    if (!input) {
        error(1, errno, "error opening file %s", path);
    }

    fseek(input, 0L, SEEK_END);
    contents.size = ftell(input);
    contents.data = malloc(contents.size + 1);

    rewind(input);
    fread(contents.data, 1, contents.size, input);
    contents.data[contents.size] = '\0';

    fclose(input);

    return contents;
}

int main(int argc, char **argv) {
    str input;
    FILE *output = NULL;

    if (argc == 1) {
        error(1, 0, "expected input file");
    }
    if (argc > 3) {
        error(1, 0, "too many arguments");
    }

    input = read_file(argv[1]);

    if (argc > 2) {
        output = fopen(argv[2], "w");
        if (!output) {
            error(1, errno, "error opening output file");
        }
    }

    run(input.data);

    for (uxx i = 0; i < static_var_count; i++) {
        str varname = static_vars[i].name;
        char *cstr = strndup(varname.data, varname.size);
        char *assign = static_vars[i].is_const ? ":=" : "=";
        printf("%s %s %ld\n", cstr, assign, static_vars[i].val);
    }
}
