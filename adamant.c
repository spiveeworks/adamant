#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <error.h>
#include <errno.h>
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>

#include "adm_types.h"

#define ARRAY_SIZE(x) (sizeof (x) / sizeof *(x))

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
    OP_MEMBER,
    OP_FUNC
} opcode;

typedef enum {
    ARG_NULL,
    ARG_GLOBAL_VAL,
    ARG_GLOBAL_DEREF,
    ARG_VAL,
    ARG_DEREF,
    ARG_CONST,
    ARG_STACK_OFFSET
} op_mode;

struct ref {
    op_mode mode;
    u64 id;
};

typedef struct instruction {
    opcode opcode;
    struct ref target;
    struct ref arg1;
    struct ref arg2;
} *Instruction;

#define STATIC_VAR_CAP 256
uxx global_var_count = 0;
uxx local_var_count = 0;

struct static_var{
    str name;
    s64 val;
    bool is_const;
} static_vars[STATIC_VAR_CAP];

u64 read_ref(struct ref ref) {
    switch (ref.mode) {
    case ARG_NULL:
        error(1, 0, "arg mode not set");
        return -1;
    case ARG_GLOBAL_VAL:
        return static_vars[ref.id].val;
    case ARG_GLOBAL_DEREF:
        /* direct access to the compiler's memory! */
        return *(u64*)static_vars[ref.id].val;
    case ARG_VAL:
        return static_vars[global_var_count + ref.id].val;
    case ARG_DEREF:
        return *(u64*)static_vars[global_var_count + ref.id].val;
    case ARG_CONST:
        return ref.id;
    case ARG_STACK_OFFSET:
        error(1, 0, "tried to read stack offset as arithmetic value");
        return -1;
    }
}

void execute_instruction(Instruction instruction) {
    u64 arg1 = read_ref(instruction->arg1);
    u64 arg2 = read_ref(instruction->arg2);

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
    case OP_FUNC:
        error(1, 0, "function call invoked as arithmetic");
    default:
        error(1, 0, "undefined/unimplemented opcode %d", instruction->opcode);
    }

    switch (instruction->target.mode) {
    case ARG_NULL:
        error(1, 0, "arg mode not set");
        break;
    case ARG_GLOBAL_VAL:
        static_vars[instruction->target.id].val = result;
        break;
    case ARG_GLOBAL_DEREF:
        *(u64*)static_vars[instruction->target.id].val = result;
        break;
    case ARG_VAL:
        static_vars[global_var_count + instruction->target.id].val = result;
        break;
    case ARG_DEREF:
        *(u64*)static_vars[global_var_count + instruction->target.id].val = result;
        break;
    case ARG_CONST:
        error(1, 0, "instruction had const target mode");
        break;
    case ARG_STACK_OFFSET:
        error(1, 0, "instruction had stack offset as target");
    }
}

sxx lookup_ident(str varname) {
    for (sxx i = global_var_count + local_var_count - 1; i >= 0; i--) {
        if (str_eq(varname, static_vars[i].name)) {
            return i;
        }
    }
    return -1;
}

void set_ref(sxx id, struct ref *out) {
    if (id < global_var_count) {
        out->mode = ARG_GLOBAL_VAL;
        out->id = id;
    } else {
        out->mode = ARG_VAL;
        out->id = id - global_var_count;
    }
}

/*************/
/* Functions */
/*************/

#define INSTRUCTION_CAP 0x100000
struct instruction instructions[INSTRUCTION_CAP];
uxx instruction_count = 0;

#define FUNC_CAP 0x400
struct func {
    Instruction istart;
    uxx length;
} funcs[FUNC_CAP];
uxx func_count = 0;

void execute_function(struct func *func, s64 offset) {
    global_var_count += offset;

    Instruction iptr = func->istart;
    Instruction end = &iptr[func->length];
    while (iptr < end) {
        if (iptr->opcode == OP_FUNC) {
            sxx func_id = read_ref(iptr->arg1);
            if (func_id < 0 || func_id >= func_count) {
                error(1, 0, "%ld is not a valid function pointer", func_id);
            }
            if (iptr->arg2.mode != ARG_STACK_OFFSET) {
                error(1, 0, "got function call without stack offset");
            }
            execute_function(&funcs[func_id], iptr->arg2.id);
        } else {
            execute_instruction(iptr);
        }
        iptr++;
    }

    global_var_count -= offset;
}

/***************/
/* Interpretor */
/***************/

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
    struct ref arg;
    opcode op;
    precedence_level precedence;
    token_id close_token;
};

#define OP_STACK_CAP 16

struct op_stack {
    uxx temp_var_count;
    sxx lhs_count;
    struct partial_instruction lhs[OP_STACK_CAP];
};

bool op_stack_can_push(struct op_stack *stack, precedence_level precedence) {
    return stack->lhs_count == 0 ||
        stack->lhs[stack->lhs_count - 1].precedence < precedence;
}

bool op_stack_can_pop_bracket(struct op_stack *stack) {
    return stack->lhs_count != 0 &&
        stack->lhs[stack->lhs_count - 1].op == OP_NULL;
}

bool op_stack_can_finish(struct op_stack *stack) {
    /* the last instruction that we emit can use any target variable we like */
    return stack->lhs_count < 2;
}

void op_stack_pop(
    struct op_stack *stack,
    struct ref *rhs,
    Instruction instruction
) {
    assert(stack->lhs_count > 0);
    stack->lhs_count--;

    uxx i = stack->lhs_count;
    instruction->opcode = stack->lhs[i].op;

    if (stack->lhs[i].close_token != '\0') {
        error(1, 0, "Excess open brackets\n");
    }
    if (stack->lhs[i].arg.mode == ARG_NULL) {
        instruction->arg1 = *rhs;
        instruction->arg2.mode = ARG_CONST;
        instruction->arg2.id = 0;
    } else {
        instruction->arg1 = stack->lhs[i].arg;
        instruction->arg2 = *rhs;
    }

    if (rhs->mode == ARG_VAL && rhs->id >= local_var_count) {
        stack->temp_var_count--;
    }
    if (stack->lhs[i].arg.mode == ARG_VAL && stack->lhs[i].arg.id >= local_var_count) {
        stack->temp_var_count--;
    }

    instruction->target.mode = ARG_VAL;
    instruction->target.id = local_var_count + stack->temp_var_count;
    stack->temp_var_count++;

    *rhs = instruction->target;
}

void op_stack_push(struct op_stack *stack, struct partial_instruction *rhs) {
    if (stack->lhs_count >= OP_STACK_CAP) {
        error(1, 0, "too many nested expressions");
    }
    stack->lhs[stack->lhs_count] = *rhs;
    stack->lhs_count += 1;
    *rhs = (struct partial_instruction){};
}

void op_stack_finish(
    struct op_stack *stack,
    struct ref target,
    struct ref rhs,
    Instruction instruction
) {
    if (stack->lhs_count == 1) {
        /* one operation remains, pop it but use our own target */
        op_stack_pop(stack, &rhs, instruction);
        stack->temp_var_count--;
        instruction->target = target;
    } else if (stack->lhs_count == 0) {
        /* no operations were added, just mov */
        instruction->opcode = OP_MOV;
        instruction->target = target;
        instruction->arg1 = rhs;
        instruction->arg2.mode = ARG_CONST;
        instruction->arg2.id = 0;
        if (rhs.mode == ARG_VAL && rhs.id >= local_var_count) {
            stack->temp_var_count--;
        }
    } else {
        error(1, 0, "can't push using step anymore");
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
    uxx i = stack->lhs_count;
    stack->lhs[i].arg = (struct ref){};
    stack->lhs[i].op = op;
    stack->lhs[i].precedence = precedence;
    stack->lhs[i].close_token = '\0';
    stack->lhs_count += 1;
}

void op_stack_push_bracket(
    struct op_stack *stack,
    token_id close_token
) {
    assert(close_token != '\0');
    if (stack->lhs_count >= OP_STACK_CAP) {
        error(1, 0, "too many nested expressions");
    }
    uxx i = stack->lhs_count;
    stack->lhs[i].arg = (struct ref){};
    stack->lhs[i].op = OP_NULL;
    stack->lhs[i].precedence = PRECEDENCE_GROUPING;
    stack->lhs[i].close_token = close_token;
    stack->lhs_count += 1;
}

void op_stack_pop_bracket(
    struct op_stack *stack,
    token_id close_token
) {
    assert(close_token != '\0');
    assert(stack->lhs_count > 0);
    stack->lhs_count--;
    uxx i = stack->lhs_count;
    if (stack->lhs[i].close_token != close_token) {
        error(1, 0, "Incorrect close bracket");
    }
    assert(stack->lhs[i].op == OP_NULL);
}

void run(char *stream) {
    struct token token;
    struct instruction instruction;
    struct func func;
    uxx func_target;
    enum {
        MODE_INTERPRET,
        MODE_BUILD_FUNC
    } interpretor_state = MODE_INTERPRET;
    enum {
        MODE_INIT,
        MODE_STATEMENT,
        MODE_IDENT,
        MODE_EXPR,
        MODE_OPERATOR,
        MODE_EXPR_FLUSH_OP,
        MODE_EXPR_FLUSH_BRACKET,
        MODE_EXPR_FLUSH_FINAL
    } parse_state = MODE_INIT;
    struct op_stack op_stack = {};
    struct ref target = {};
    struct partial_instruction rhs = {};
    str varname;
    token_id close_token;
    bool is_var_decl;

    while (true) {
        instruction.opcode = OP_NULL;

        switch (parse_state) {
        case MODE_INIT:
            assert(op_stack.lhs_count == 0);
            assert(op_stack.temp_var_count == 0);
            is_var_decl = false;
            close_token = '\0';
            parse_state = MODE_STATEMENT;
            break;
        case MODE_STATEMENT:
            stream = read_token(stream, &token);
            switch (token.id) {
            case '\0':
                if (interpretor_state == MODE_BUILD_FUNC) {
                    error(1, 0, "Got to end of file before end of function definition");
                }
                return;
            case TOK_IDENT:
                varname = token.substr;
                parse_state = MODE_IDENT;
                break;
            case TOK_VAR:
                if (is_var_decl) {
                    error(1, 0, "repeated \"var\" keyword");
                }
                is_var_decl = true;
                break;
            case TOK_FUNC:
                if (interpretor_state != MODE_INTERPRET) {
                    error(1, 0, "function definition outside of global scope");
                }
                stream = read_token(stream, &token);
                if (token.id != TOK_IDENT) {
                    error(1, 0, "expected identifier, got \"%s\"", cstr(token.substr));
                }
                static_vars[global_var_count].name = token.substr;
                static_vars[global_var_count].val = -1;
                static_vars[global_var_count].is_const = true;
                func_target = global_var_count;
                global_var_count++;
                stream = read_token(stream, &token);
                if (token.id != '(') {
                    error(1, 0, "expected '(', got \"%s\"", cstr(token.substr));
                }
                stream = read_token(stream, &token);
                if (token.id != ')') {
                    error(1, 0, "expected ')', got \"%s\"", cstr(token.substr));
                }
                stream = read_token(stream, &token);
                if (token.id != '{') {
                    error(1, 0, "expected '{', got \"%s\"", cstr(token.substr));
                }
                func.istart = &instructions[instruction_count];
                func.length = 0;
                interpretor_state = MODE_BUILD_FUNC;
                break;
            case '}':
                if (interpretor_state == MODE_INTERPRET) {
                    error(1, 0, "expected static statement, got '}'");
                }
                static_vars[func_target].val = func_count;
                funcs[func_count] = func;
                func_count++;
                local_var_count = 0;
                interpretor_state = MODE_INTERPRET;
                break;
            default:
                error(1, 0, "currently only assignment/declaration statements are supported");
            }
            break;

        case MODE_IDENT:
            stream = read_token(stream, &token);
            if (token.id == '(') {
                stream = read_token(stream, &token);
                if (token.id != ')') {
                    error(1, 0, "expected ')', got \"%s\"", cstr(token.substr));
                }
                stream = read_token(stream, &token);
                if (token.id != ';') {
                    error(1, 0, "expected ';', got \"%s\"", cstr(token.substr));
                }
                sxx var_id = lookup_ident(varname);
                if (var_id == -1) {
                    error(1, 0, "unknown function name %s", cstr(varname));
                }
                instruction.opcode = OP_FUNC;
                set_ref(var_id, &instruction.arg1);
                instruction.arg2.mode = ARG_STACK_OFFSET;
                instruction.arg2.id = global_var_count + local_var_count;
                parse_state = MODE_INIT;
                break;
            }
            if (token.id != TOK_DEFINE && token.id != '=') {
                error(1, 0, "expected '=' or ':=', got \"%s\"", cstr(token.substr));
            }
            if (token.id == TOK_DEFINE || is_var_decl) {
                uxx id;
                if (interpretor_state == MODE_BUILD_FUNC) {
                    id = global_var_count + local_var_count;
                    target.id = local_var_count;
                    target.mode = ARG_VAL;
                    local_var_count++;
                } else {
                    id = global_var_count;
                    target.id = global_var_count;
                    target.mode = ARG_GLOBAL_VAL;
                    global_var_count++;
                }
                static_vars[id].name = varname;
                static_vars[id].val = 0;
                static_vars[id].is_const = !is_var_decl;
            } else {
                sxx id = lookup_ident(varname);
                if (id == -1) {
                    error(1, 0, "undefined identifier '%s'", cstr(varname));
                }
                set_ref(id, &target);
            }
            parse_state = MODE_EXPR;
            break;

        case MODE_EXPR:
            if (rhs.arg.mode != ARG_NULL) {
                error(1, 0, "parse state error");
            }
            stream = read_token(stream, &token);
            switch (token.id) {
            case TOK_NUM:
                rhs.arg.mode = ARG_CONST;
                rhs.arg.id = token.val;
                parse_state = MODE_OPERATOR;
                break;
            case TOK_IDENT: {
                sxx id = lookup_ident(token.substr);
                if (id == -1) {
                    error(1, 0, "undefined identifier '%s'", cstr(token.substr));
                }
                set_ref(id, &rhs.arg);
                varname = token.substr;
                parse_state = MODE_OPERATOR;
                break;
            }
            case '(':
                op_stack_push_bracket(&op_stack, ')');
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
                rhs.op = OP_NULL;
                rhs.precedence = PRECEDENCE_GROUPING;
                close_token = ')';
                parse_state = MODE_EXPR_FLUSH_BRACKET;
            } else if (token.id == ';') {
                parse_state = MODE_EXPR_FLUSH_FINAL;
            } else {
                bool found = false;
                for (uxx i = 0; i < OPERATOR_COUNT; i++) {
                    if (token.id != operators[i].tok) continue;

                    found = true;
                    rhs.op = operators[i].op;
                    rhs.precedence = operators[i].precedence;
                    parse_state = MODE_EXPR_FLUSH_OP;
                    break;
                }
                if (!found) {
                    error(1, 0, "expected binary operator or ';', got \"%s\"", cstr(token.substr));
                }
            }
            break;
        case MODE_EXPR_FLUSH_OP:
            if (op_stack_can_push(&op_stack, rhs.precedence)) {
                op_stack_push(&op_stack, &rhs);
                parse_state = MODE_EXPR;
            } else {
                op_stack_pop(&op_stack, &rhs.arg, &instruction);
            }
            break;
        case MODE_EXPR_FLUSH_BRACKET:
            if (op_stack.lhs_count == 0) {
                error(1, 0, "Excess close brackets");
            }
            if (op_stack_can_pop_bracket(&op_stack)) {
                op_stack_pop_bracket(&op_stack, close_token);
                parse_state = MODE_OPERATOR;
            } else {
                op_stack_pop(&op_stack, &rhs.arg, &instruction);
            }
            break;
        case MODE_EXPR_FLUSH_FINAL:
            if (op_stack_can_finish(&op_stack)) {
                op_stack_finish(&op_stack, target, rhs.arg, &instruction);
                target = (struct ref){};
                rhs = (struct partial_instruction){};
                parse_state = MODE_INIT;
            } else {
                op_stack_pop(&op_stack, &rhs.arg, &instruction);
            }
            break;
        default:
            error(1, 0, "parse state corrupted");
            break;
        }

        if (instruction.opcode == OP_NULL) continue;

        switch(interpretor_state) {
        case MODE_INTERPRET:
            if (instruction.opcode == OP_FUNC) {
                sxx func_id = read_ref(instruction.arg1);
                if (func_id < 0 || func_id >= func_count) {
                    error(1, 0, "%s is not a valid function pointer", cstr(varname));
                }
                if (instruction.arg2.mode != ARG_STACK_OFFSET) {
                    error(1, 0, "got function call without stack offset");
                }
                execute_function(&funcs[func_id], instruction.arg2.id);
            } else {
                execute_instruction(&instruction);
            }
            break;
        case MODE_BUILD_FUNC:
            func.istart[func.length++] = instruction;
            instruction_count++;
            break;
        default:
            error(1, 0, "interpretor state corrupted");
        }
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

    for (uxx i = 0; i < global_var_count; i++) {
        str varname = static_vars[i].name;
        char *cstr = strndup(varname.data, varname.size);
        char *assign = static_vars[i].is_const ? ":=" : "=";
        printf("%s %s %ld\n", cstr, assign, static_vars[i].val);
    }
}
