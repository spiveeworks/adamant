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
    TOK_LEQ,
    TOK_GEQ,
    TOK_LSHIFT,
    TOK_RSHIFT,
    TOK_FUNC
} token_id;

#define COMPOUND_OPERATOR_COUNT 8
const struct compound_operators {
    token_id id;
    char *word;
} compound_operators[COMPOUND_OPERATOR_COUNT] = {
    {TOK_DEFINE, ":="},
    {TOK_LOGIC_OR, "||"},
    {TOK_LOGIC_AND, "&&"},
    {TOK_EQ, "=="},
    {TOK_LEQ, "<="},
    {TOK_GEQ, ">="},
    {TOK_LSHIFT, "<<"},
    {TOK_RSHIFT, ">>"}
};

#define KEYWORD_COUNT 1
const struct keywords {
    token_id id;
    char *word;
} keywords[KEYWORD_COUNT] = {
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
    OP_MEMBER
} opcode;

typedef enum {
    ARG_NULL,
    ARG_VAL,
    ARG_DEREF,
    ARG_CONST,
} op_mode;

struct instruction {
    opcode opcode;
    op_mode target_mode;
    op_mode arg1_mode;
    op_mode arg2_mode;
    u64 target;
    u64 arg1;
    u64 arg2;
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
    switch (instruction->arg1_mode) {
    case ARG_VAL:
        arg1 = static_vars[instruction->arg1].val;
        break;
    case ARG_DEREF:
        /* direct access to the compiler's memory! */
        arg1 = *(u64*)static_vars[instruction->arg1].val;
        break;
    case ARG_CONST:
        arg1 = instruction->arg1;
        break;
    case ARG_NULL:
        error(1, 0, "arg mode not set");
        break;
    }

    u64 arg2 = 0;
    switch (instruction->arg2_mode) {
    case ARG_VAL:
        arg2 = static_vars[instruction->arg2].val;
        break;
    case ARG_DEREF:
        /* direct access to the compiler's memory! */
        arg2 = *(u64*)static_vars[instruction->arg2].val;
        break;
    case ARG_CONST:
        arg2 = instruction->arg2;
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
    case OP_MEMBER:
        error(1, 0, "member operator not yet implemented");
        break;
    }

    switch (instruction->target_mode) {
    case ARG_VAL:
        static_vars[instruction->target].val = result;
        break;
    case ARG_DEREF:
        *(u64*)static_vars[instruction->target].val = result;
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
    PRECEDENCE_DISJUNCTIVE,
    PRECEDENCE_CONJUNCTIVE,
    PRECEDENCE_COMPARATIVE,
    PRECEDENCE_ADDITIVE,
    PRECEDENCE_MULTIPLICATIVE,
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
    bool is_temp;
    opcode op;
    op_mode mode;
    u64 arg;
};

struct op_stack {
    op_mode target_mode;
    u64 target;
    struct partial_instruction lhs[PRECEDENCE_COUNT];
    struct partial_instruction rhs;
    precedence_level next_precedence;
};

void op_stack_check(struct op_stack *stack) {
    if (stack->target_mode != ARG_NULL) {
        error(0, 0, "warning: stack target mode was not null");
    }
    if (stack->target != 0) {
        error(0, 0, "warning: stack target val was not null");
    }
    for (sxx i = 0; i < PRECEDENCE_COUNT; i++) {
        if (stack->lhs[i].is_temp != false) {
            error(0, 0, "warning: stack flag %ld was not null", i);
        }
        if (stack->lhs[i].op != OP_NULL) {
            error(0, 0, "warning: stack op %ld was not null", i);
        }
        if (stack->lhs[i].mode != ARG_NULL) {
            error(0, 0, "warning: stack mode %ld was not null", i);
        }
        if (stack->lhs[i].arg != 0) {
            error(0, 0, "warning: stack val %ld was not null", i);
        }
    }
    if (stack->rhs.is_temp != false) {
        error(0, 0, "warning: stack top flag was not null");
    }
    if (stack->rhs.mode != ARG_NULL) {
        error(0, 0, "warning: stack top mode was not null");
    }
    if (stack->rhs.arg != 0) {
        error(0, 0, "warning: stack top val was not null");
    }
    if (stack->rhs.op != OP_NULL) {
        error(0, 0, "warning: stack top op was not null");
    }
    if (stack->next_precedence != PRECEDENCE_DISJUNCTIVE) {
        error(0, 0, "warning: stack precedence was not null");
    }
}

void op_stack_step(struct op_stack *stack, struct instruction *instruction) {
    for (sxx i = PRECEDENCE_COUNT - 1; i >= stack->next_precedence; i--) {
        if (stack->lhs[i].op == OP_NULL) continue;

        instruction->opcode = stack->lhs[i].op;
        instruction->arg1_mode = stack->lhs[i].mode;
        instruction->arg1 = stack->lhs[i].arg;
        instruction->arg2_mode = stack->rhs.mode;
        instruction->arg2 = stack->rhs.arg;

        if (stack->lhs[i].is_temp) {
            static_var_count--;
        }
        if (stack->rhs.is_temp) {
            static_var_count--;
        }

        stack->lhs[i].is_temp = false;
        stack->lhs[i].op = OP_NULL;
        stack->lhs[i].mode = ARG_NULL;
        stack->lhs[i].arg = 0;

        bool stack_empty = false;
        if (stack->rhs.op == OP_NULL) {
            stack_empty = true;
            for (sxx j = i - 1; j >= 0; j--) {
                if (stack->lhs[j].op != OP_NULL) {
                    stack_empty = false;
                }
            }
        }

        if (stack_empty) {
            /* finished flushing stack */
            instruction->target_mode = stack->target_mode;
            instruction->target = stack->target;
            stack->target_mode = ARG_NULL;
            stack->target = 0;
            stack->rhs.is_temp = false;
            stack->rhs.mode = ARG_NULL;
            stack->rhs.arg = 0;
            stack->next_precedence = 0;
        } else {
            /* output a temporary and continue */
            instruction->target_mode = ARG_VAL;
            instruction->target = static_var_count;
            stack->rhs.is_temp = true;
            stack->rhs.mode = ARG_VAL;
            stack->rhs.arg = static_var_count;
            static_var_count++;
        }

        return;
    }

    if (stack->rhs.op == OP_NULL) {
        /* no operations were added, just mov */
        instruction->opcode = OP_MOV;
        instruction->target = stack->target;
        instruction->target_mode = stack->target_mode;
        instruction->arg1_mode = stack->rhs.mode;
        instruction->arg1 = stack->rhs.arg;
        instruction->arg2_mode = ARG_CONST;
        instruction->arg2 = 0;
        if (stack->rhs.is_temp) static_var_count--;
        stack->target_mode = 0;
        stack->target = 0;
    } else {
        /* stack has been flushed past desired precedence level, push */
        stack->lhs[stack->next_precedence] = stack->rhs;
    }

    stack->rhs.is_temp = false;
    stack->rhs.op = OP_NULL;
    stack->rhs.mode = ARG_NULL;
    stack->rhs.arg = 0;
    stack->next_precedence = 0;
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
        MODE_EXPR_FLUSH,
        MODE_EXPR_FLUSH_FINAL
    } mode = MODE_INIT;
    struct op_stack op_stack;
    str varname;

    memset(&op_stack, 0, sizeof(struct op_stack));

    while (true) {
        instruction.opcode = OP_NULL;

        switch (mode) {
        case MODE_INIT:
            op_stack_check(&op_stack);
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
            default:
                error(1, 0, "currently only assignment/declaration statements are supported");
            }
            break;

        case MODE_IDENT:
            stream = read_token(stream, &token);
            if (token.id != TOK_DEFINE) {
                error(1, 0, "unexpected token %s", cstr(token.substr));
            }
            static_vars[static_var_count].name = varname;
            static_vars[static_var_count].val = 0;
            op_stack.target = static_var_count;
            op_stack.target_mode = ARG_VAL;
            static_var_count++;
            mode = MODE_EXPR;
            break;

        case MODE_EXPR:
            if (op_stack.rhs.mode != ARG_NULL) {
                error(1, 0, "parse state error");
            }
            stream = read_token(stream, &token);
            switch (token.id) {
            case TOK_NUM:
                op_stack.rhs.is_temp = false;
                op_stack.rhs.mode = ARG_CONST;
                op_stack.rhs.arg = token.val;
                break;
            default:
                error(1, 0, "expected number, got \"%s\"", cstr(token.substr));
                break;
            }
            mode = MODE_OPERATOR;
            break;

        case MODE_OPERATOR:
            stream = read_token(stream, &token);
            if (token.id == ';') {
                mode = MODE_EXPR_FLUSH_FINAL;
            } else {
                bool found = false;
                for (uxx i = 0; i < OPERATOR_COUNT; i++) {
                    if (token.id != operators[i].tok) continue;

                    found = true;
                    op_stack.rhs.op = operators[i].op;
                    op_stack.next_precedence = operators[i].precedence;
                    break;
                }
                if (!found) {
                    error(1, 0, "expected binary operator or ';', got \"%s\"", cstr(token.substr));
                }
                mode = MODE_EXPR_FLUSH;
            }
            break;
        case MODE_EXPR_FLUSH:
            op_stack_step(&op_stack, &instruction);
            if (op_stack.rhs.mode == ARG_NULL) {
                mode = MODE_EXPR;
            }
            break;
        case MODE_EXPR_FLUSH_FINAL:
            op_stack_step(&op_stack, &instruction);
            if (op_stack.rhs.mode == ARG_NULL) {
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
        printf("%s\t:= %ld\n", cstr, static_vars[i].val);
    }
}
