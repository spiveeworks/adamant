#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <error.h>
#include <errno.h>
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>

#include <sys/stat.h>

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
    TOK_LOCAL,
    TOK_FUNC,
    TOK_PROC
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

#define KEYWORD_COUNT 4
const struct keywords {
    token_id id;
    char *word;
} keywords[KEYWORD_COUNT] = {
    {TOK_VAR, "var"},
    {TOK_LOCAL, "local"},
    {TOK_FUNC, "func"},
    {TOK_PROC, "proc"}
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
    /* arithmetic instructions */
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
    OP_EXIT,
    /* for interpretor use */
    OP_FUNC,
    /* for parser use */
    OP_DEREF
} opcode;

#define INTRINSIC_COUNT 1
const struct intrinsics {
    opcode opcode;
    char *word;
} intrinsics[INTRINSIC_COUNT] = {
    {OP_EXIT, "exit"}
};

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

typedef enum {
    DECL_VAL,
    DECL_VAR,
    DECL_LOCAL,
    DECL_WRITE
} decl_type;

struct static_var {
    str name;
    s64 val;
    decl_type decl_type;
} static_vars[STATIC_VAR_CAP];

#define MEM_STACK_CAP 256
u64 mem_stack[MEM_STACK_CAP];
uxx mem_stack_count = 0;
u64 * const MEM_START = mem_stack;
u64 * const MEM_END = mem_stack + MEM_STACK_CAP;

u64 *lookup(u64 addr) {
    u64 *ptr = (u64*)addr;
    if (ptr < MEM_START || ptr + 1 >= MEM_END) {
        error(1, 0, "Attempted to dereference %p, outside of allowed memory", ptr);
    }
    return ptr;
}

u64 read_ref(struct ref ref) {
    u64 *loc;
    switch (ref.mode) {
    case ARG_NULL:
        error(1, 0, "arg mode not set");
        return -1;
    case ARG_GLOBAL_VAL:
        return static_vars[ref.id].val;
    case ARG_GLOBAL_DEREF:
        return *lookup(static_vars[ref.id].val);
    case ARG_VAL:
        return static_vars[global_var_count + ref.id].val;
    case ARG_DEREF:
        return *lookup(static_vars[global_var_count + ref.id].val);
    case ARG_CONST:
        return ref.id;
    case ARG_STACK_OFFSET:
        error(1, 0, "tried to read stack offset as arithmetic value");
        return -1;
    }
}

void execute_instruction(Instruction instruction) {
    u64 arg1;
    u64 arg2;
    if (instruction->arg1.mode == ARG_STACK_OFFSET) {
        arg1 = static_vars[global_var_count + instruction->arg1.id].val;
        arg2 = static_vars[global_var_count + instruction->arg1.id + 1].val;
    } else {
        arg1 = read_ref(instruction->arg1);
        arg2 = read_ref(instruction->arg2);
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
    case OP_EXIT:
        exit(arg1);
    case OP_FUNC:
        error(1, 0, "function call invoked as arithmetic");
    case OP_DEREF:
        error(1, 0, "deref operator invoked as arithmetic");
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
        *lookup(static_vars[instruction->target.id].val)
                = result;
        break;
    case ARG_VAL:
        static_vars[global_var_count + instruction->target.id].val = result;
        break;
    case ARG_DEREF:
        *lookup(static_vars[global_var_count + instruction->target.id].val)
            = result;
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

opcode lookup_intrinsic(str varname) {
    for (sxx i = 0; i < INTRINSIC_COUNT; i++) {
        if (strncmp(varname.data, intrinsics[i].word, varname.size) == 0) {
            return intrinsics[i].opcode;
        }
    }
    return OP_NULL;
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

void set_ref_write(sxx id, struct ref *out) {
    if (id < global_var_count) {
        out->mode = ARG_GLOBAL_DEREF;
        out->id = id;
    } else {
        out->mode = ARG_DEREF;
        out->id = id - global_var_count;
    }
}

/*************/
/* Functions */
/*************/

#define INSTRUCTION_CAP 0x100000
struct instruction instructions[INSTRUCTION_CAP];
uxx instruction_count = 0;

enum func_modifier {
    FUNC_DYNAMIC,
    FUNC_PROC
    /* , FUNC_STATIC, FUNC_CODEGEN */
};

#define FUNC_CAP 0x400
struct func {
    Instruction istart;
    uxx length;
    enum func_modifier mod;
    /* this is for type checking, not for execution */
    uxx num_params;
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

/************/
/* Compiler */
/************/

/* taken from https://github.com/corkami/pics/blob/master/binary/elf101 */
#define HEADER_BINARY_SIZE 96
const b8 header_binary[HEADER_BINARY_SIZE] = {
    0x7F, 0x45, 0x4C, 0x46, 0x01, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x03, 0x00, 0x01, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x08, 0x40, 0x00, 0x00, 0x00,
    0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x34, 0x00, 0x20, 0x00, 0x01, 0x00, 0x28, 0x00,
    0x04, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x08,
    0xA0, 0x00, 0x00, 0x00, 0xA0, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

const u64 elf_e_entry_location = 0x18;
const u64 elf_e_shoff_location = 0x20;
const u64 elf_p_filesz_location = 0x50;
const u64 elf_p_memsz_location = 0x54;

void put32(b32 x, FILE *out) {
    putc(x & 0xFF, out);
    x >>= 8;
    putc(x & 0xFF, out);
    x >>= 8;
    putc(x & 0xFF, out);
    x >>= 8;
    putc(x, out);
}

void put16(b16 x, FILE *out) {
    putc(x & 0xFF, out);
    x >>= 8;
    putc(x, out);
}

void putbytes(const b8 *x, u64 size, FILE *out) {
    for (u64 i = 0; i < size; i++) putc(x[i], out);
}

u64 elf_text_offset = 0;
u64 elf_text_size = 0;
u64 elf_data_size = 0;

FILE* start_elf(char *path) {
    FILE *out = fopen(path, "wb");
    if (!out) {
        error_at_line(1, errno, __FILE__, __LINE__, "Failed to open \"%s\" for writing.", path);
    }
    if (chmod(path, 0755) < 0) {
        error_at_line(1, 0, __FILE__, __LINE__, "Failed to change permissions for \"%s\".", path);
    }

    putbytes(header_binary, HEADER_BINARY_SIZE, out);

    elf_text_offset = ftell(out);

    return out;
}

struct var_state {
    u64 offset;
    bool reg;
    bool initialised;
} var_state[STATIC_VAR_CAP];
enum reg {
    REG_EAX,
    REG_ECX,
    REG_EDX,
    REG_EBX,
};
struct reg_state {
    s64 var;
    bool initialised;
} reg_state[4];
struct local_state {
    s64 var;
} local_state[STATIC_VAR_CAP];

void compile_proc(struct func *proc, bool is_entry_point, FILE *out) {
    if (!is_entry_point) error(1, 0, "Currently the only proc must be main.");

    for (sxx i = 0; i < STATIC_VAR_CAP; i++) {
        var_state[i] = (struct var_state){};
        local_state[i].var = -1;
    }
    for (sxx i = 0; i < 4; i++) {
        reg_state[i].var = -1;
    }

    Instruction istart = proc->istart;
    u64 icount = proc->length;

    for (u64 i = 0; i < icount; i++) {
        struct instruction instr = istart[i];
        if (instr.opcode == OP_EXIT) {
            assert(instr.arg1.mode == ARG_STACK_OFFSET);
            sxx var = instr.arg1.id;
            if (!var_state[var].initialised) {
                error(1, 0, "exit with uninitialised variable");
            }
            assert(var_state[var].reg);
            sxx reg = var_state[var].offset;
            if (reg != REG_EBX) {
                /* mov ebx, var ; return code*/
                putc(0x89, out);
                putc(0300 | (reg << 3) | REG_EBX, out);
                printf("mov ebx, %%%lu\n", var);
            }
            /* mov eax, 1   ; SC_EXIT*/
            putc(0xB8, out);
            put32(1, out);
            /* int 0x80     ; system call*/
            putc(0xCD, out);
            putc(0x80, out);
        } else if (instr.opcode == OP_FUNC) {
            error(1, 0, "tried to compile function application");
        } else {
            s32 reg = -1;
            if ((instr.target.mode == ARG_VAL && instr.arg2.mode == ARG_VAL
                    && instr.target.id == instr.arg2.id)
                || (instr.arg1.mode == ARG_CONST
                    && instr.arg2.mode != ARG_CONST))
            {
                struct ref swp = instr.arg1;
                instr.arg1 = instr.arg2;
                instr.arg2 = swp;
            }
            if (instr.target.mode == ARG_VAL && instr.arg1.mode == ARG_VAL
                && instr.target.id == instr.arg1.id)
            {
                if (!var_state[instr.arg1.id].initialised) {
                    error(1, 0, "reading from uninitialised variable %lu",
                        instr.arg1.id);
                }
                if (!var_state[instr.arg1.id].reg) {
                    error(1, 0, "reading from non-register value");
                }
                reg = var_state[instr.arg1.id].offset;
            } else if (instr.target.mode == ARG_VAL
                && var_state[instr.target.id].reg)
            {
                /* eventually there will be lots of ways for registers to
                   unbind, but this will do for now. */
                reg = var_state[instr.target.id].offset;
            } else if (instr.target.mode == ARG_VAL) {
                for (u8 j = 0; j < 4; j++) {
                    if (reg_state[j].var == -1) {
                        reg = j;
                        break;
                    }
                }
                if (reg == -1) {
                    error(1, 0, "all registers are full");
                }
            } else {
                error(1, 0, "addresses are not yet implemented");
            }
            /* MOV first argument, possibly in preparation for binary ops */
            {
                if (instr.arg1.mode == ARG_CONST) {
                    putc(0xB8 + reg, out);
                    if (instr.arg1.id > 0xFFFFFFFF) {
                        error(1, 0, "values must be 32 bit at this time");
                    }
                    put32(instr.arg1.id, out);
                    printf("mov %%%lu, %lu\n", instr.target.id, instr.arg1.id);
                } else if (instr.arg1.mode == ARG_VAL) {
                    if (!var_state[instr.arg1.id].initialised) {
                        error(1, 0, "reading from uninitialised variable %lu",
                            instr.arg1.id);
                    }
                    if (!var_state[instr.arg1.id].reg) {
                        error(1, 0, "reading from non-register value");
                    }
                    s32 read_reg = var_state[instr.arg1.id].offset;
                    if (reg != read_reg) {
                        putc(0x89, out);
                        putc(0300 | (read_reg << 3) | reg, out);
                        printf("mov %%%lu, %%%lu\n", instr.target.id, instr.arg1.id);
                    }
                } else {
                    error(1, 0, "addresses are not yet implemented");
                }
                reg_state[reg].var = instr.target.id;
                var_state[instr.target.id].offset = reg;
                var_state[instr.target.id].reg = true;
                var_state[instr.target.id].initialised = true;
            }
            s8 simple_opcode = -1;
            if (instr.opcode == OP_ADD) simple_opcode = 0;
            if (instr.opcode == OP_OR) simple_opcode = 1;
            if (instr.opcode == OP_AND) simple_opcode = 4;
            if (instr.opcode == OP_SUB) simple_opcode = 5;
            if (instr.opcode == OP_XOR) simple_opcode = 6;
            char *simple_mnemonics[8] = {"add", "or", "adc", "sbb", "and",
                "sub", "xor", "cmp"};
            if (simple_opcode != -1) {
                if (instr.arg2.mode == ARG_CONST) {
                    putc(0x81, out);
                    putc(0300 | (simple_opcode << 3) | reg, out);
                    if (instr.arg2.id > 0xFFFFFFFF) {
                        error(1, 0, "values must be 32 bit at this time");
                    }
                    put32(instr.arg2.id, out);
                    printf("%s %%%lu, %lu\n", simple_mnemonics[simple_opcode], instr.target.id, instr.arg2.id);
                } else if (instr.arg2.mode == ARG_VAL) {
                    if (!var_state[instr.arg2.id].initialised) {
                        error(1, 0, "reading from uninitialised variable %lu",
                            instr.arg2.id);
                    }
                    if (!var_state[instr.arg2.id].reg) {
                        error(1, 0, "reading from non-register value");
                    }
                    s32 read_reg = var_state[instr.arg2.id].offset;
                    putc(0001 | (simple_opcode << 3), out);
                    putc(0300 | (read_reg << 3) | reg, out);
                    printf("%s %%%lu, %%%lu\n", simple_mnemonics[simple_opcode], instr.target.id, instr.arg2.id);
                } else {
                    error(1, 0, "addresses are not yet implemented");
                }
            } else if (instr.opcode != OP_MOV) {
                error(1, 0, "Opcode %d not yet supported in compilation", instr.opcode);
            }
        }
    }
    printf("\n");
}

void compile(FILE *out) {
    int entry_point = 0;
    str entrypoint_name = {4, "main"};
    for (u64 i = 0; i < global_var_count; i++) {
        if (str_eq(static_vars[i].name, entrypoint_name)) {
            entry_point = i;
        }
    }
    for (u64 i = 0; i < func_count; i++) {
        if (funcs[i].mod == FUNC_PROC) {
            compile_proc(&funcs[i], i == entry_point, out);
        }
    }
}

#define SECTION_NAMES_BINARY_SIZE 32
const b8 section_names_binary[SECTION_NAMES_BINARY_SIZE] = {
    0x00, 0x2E, 0x73, 0x68, 0x73, 0x74, 0x72, 0x74, 0x61, 0x62, 0x00, 0x2E, 0x74, 0x65, 0x78, 0x74,
    0x00, 0x2E, 0x72, 0x6F, 0x64, 0x61, 0x74, 0x61, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
};

void end_elf(FILE *out) {
    u64 elf_rodata_offset = ftell(out);
    elf_text_size = elf_rodata_offset - elf_text_offset;
    assert(elf_data_size == 0);
    /*
    putbytes(data_binary, elf_data_size, out);
    */

    u64 p_filesz = ftell(out);

    u64 elf_shrtrtab_offset = ftell(out);
    putbytes(section_names_binary, SECTION_NAMES_BINARY_SIZE, out);


    /* section header table (nobody really needs this) */
    u64 elf_sh_offset = ftell(out);

    for (u64 i = 0; i < 40; i++) putc(0, out); /* null table entry */

    put32(11, out); /* .text */
    put32(1, out);  /* SHT_PROGBITS */
    put32(6, out);  /* SHF_ALLOC | SHF_EXECINSTR */
    put32(0x8000000 + elf_text_offset, out); /* sh_addr */
    put32(elf_text_offset, out); /* sh_offset */
    put32(elf_text_size, out);   /* sh_size */
    for (u64 i = 0; i < 16; i++) putc(0, out);

    put32(17, out); /* .rodata */
    put32(1, out);  /* SHT_PROGBITS */
    put32(2, out);  /* SHF_ALLOC */
    put32(0x8000000 + elf_rodata_offset, out); /* sh_addr */
    put32(elf_rodata_offset, out); /* sh_offset */
    put32(elf_data_size, out);   /* sh_size */
    for (u64 i = 0; i < 16; i++) putc(0, out);

    put32(1, out);  /* .shrtrtab */
    put32(3, out);  /* SHT_STRTAB */
    put32(0, out);
    put32(0, out);
    put32(elf_shrtrtab_offset, out); /* sh_offset */
    put32(SECTION_NAMES_BINARY_SIZE, out);   /* sh_size */
    for (u64 i = 0; i < 16; i++) putc(0, out);

    fseek(out, elf_e_entry_location, SEEK_SET);
    put32(0x8000000 + elf_text_offset, out);

    fseek(out, elf_e_shoff_location, SEEK_SET);
    put32(elf_sh_offset, out);

    fseek(out, elf_p_filesz_location, SEEK_SET);
    put32(p_filesz, out);

    fseek(out, elf_p_memsz_location, SEEK_SET);
    put32(p_filesz, out);

    fclose(out);
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

bool op_stack_must_copy(struct op_stack *stack, struct ref rhs) {
    return stack->lhs_count == 0 &&
        (rhs.mode != ARG_VAL || rhs.id < local_var_count);
}

bool op_stack_can_finish(struct op_stack *stack) {
    /* the last instruction that we emit can use any target variable we like */
    return stack->lhs_count == 0 ||
        (stack->lhs_count == 1 && stack->lhs[0].op != OP_DEREF);
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

    if (stack->lhs[i].op == OP_DEREF) {
        if (rhs->mode == ARG_VAL) {
            rhs->mode = ARG_DEREF;
            instruction->opcode = OP_NULL;
            return;
        } /* else */
        if (rhs->mode == ARG_GLOBAL_VAL) {
            rhs->mode = ARG_GLOBAL_DEREF;
            instruction->opcode = OP_NULL;
            return;
        } /* else */
        instruction->opcode = OP_MOV;
        instruction->arg1 = *rhs;
        instruction->arg2.mode = ARG_CONST;
        instruction->arg2.id = 0;
        /* keep op on the stack, to try again next time */
        stack->lhs_count++;
    } else if (stack->lhs[i].arg.mode == ARG_NULL) {
        instruction->arg1 = *rhs;
        instruction->arg2.mode = ARG_CONST;
        instruction->arg2.id = 0;
    } else {
        instruction->arg1 = stack->lhs[i].arg;
        instruction->arg2 = *rhs;
    }

    if ((rhs->mode == ARG_VAL || rhs->mode == ARG_DEREF)
        && rhs->id >= local_var_count)
    {
        stack->temp_var_count--;
    }
    if ((stack->lhs[i].arg.mode == ARG_VAL || rhs->mode == ARG_DEREF)
        && stack->lhs[i].arg.id >= local_var_count)
    {
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

void op_stack_copy(
    struct op_stack *stack,
    struct ref *rhs,
    Instruction instruction
) {
    instruction->opcode = OP_MOV;
    instruction->arg1 = *rhs;
    instruction->arg2.mode = ARG_CONST;
    instruction->arg2.id = 0;
    struct ref target;
    target.mode = ARG_VAL;
    target.id = local_var_count + stack->temp_var_count;
    instruction->target = target;
    *rhs = target;
    stack->temp_var_count++;
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
        MODE_EXPR_FLUSH_COMMA,
        MODE_EXPR_FLUSH_FINAL
    } parse_state = MODE_INIT;
    struct op_stack op_stack = {};
    struct ref target = {};
    struct partial_instruction rhs = {};
    str varname;
    token_id close_token;
    bool is_func_statement = false;
    opcode func_statement_op;
    sxx func_statement_var_id = -1;
    /* at some point this will need to be on the parse stack */
    /* not to be confused with C++ decltype */
    decl_type decl_type;

    while (true) {
        instruction.opcode = OP_NULL;

        switch (parse_state) {
        case MODE_INIT:
            assert(op_stack.lhs_count == 0);
            assert(op_stack.temp_var_count == 0);
            decl_type = DECL_VAL;
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
                if (decl_type != DECL_VAL) {
                    error(1, 0, "multiple declaration keywords in a row");
                }
                decl_type = DECL_VAR;
                break;
            case TOK_LOCAL:
                if (decl_type != DECL_VAL) {
                    error(1, 0, "multiple declaration keywords in a row");
                }
                decl_type = DECL_LOCAL;
                if (interpretor_state != MODE_INTERPRET) {
                    error_at_line(1, 0, __FILE__, __LINE__, "local variables in function definitions (ironically) not yet implemented");
                }
                break;
            case '*':
                if (decl_type != DECL_VAL) {
                    error(1, 0, "expected identifier, got '*'");
                }
                /* FIXME `*var x = 0;` etc. will give weird error messages */
                decl_type = DECL_WRITE;
                break;
            case TOK_FUNC:
            case TOK_PROC:
                if (interpretor_state != MODE_INTERPRET) {
                    error(1, 0, "function definition outside of global scope");
                }
                bool is_proc = token.id == TOK_PROC;
                stream = read_token(stream, &token);
                if (token.id != TOK_IDENT) {
                    error(1, 0, "expected identifier, got \"%s\"", cstr(token.substr));
                }
                static_vars[global_var_count].name = token.substr;
                static_vars[global_var_count].val = -1;
                static_vars[global_var_count].decl_type = DECL_VAL;
                func_target = global_var_count;
                global_var_count++;
                stream = read_token(stream, &token);
                if (token.id != '(') {
                    error(1, 0, "expected '(', got \"%s\"", cstr(token.substr));
                }
                func.num_params = 0;
                while (true) {
                    stream = read_token(stream, &token);
                    if (token.id == ')') break;
                    if (token.id != TOK_IDENT) {
                        error(1, 0, "expected identifier, or ')', got \"%s\"", cstr(token.substr));
                    }
                    func.num_params += 1;
                    uxx id = global_var_count + local_var_count;
                    static_vars[id].name = token.substr;
                    local_var_count++;
                    stream = read_token(stream, &token);
                    if (token.id == ')') break;
                    if (token.id != ',') {
                        error(1, 0, "expected ',', or ')', got \"%s\"", cstr(token.substr));
                    }
                }
                stream = read_token(stream, &token);
                if (token.id != '{') {
                    error(1, 0, "expected '{', got \"%s\"", cstr(token.substr));
                }
                func.istart = &instructions[instruction_count];
                func.length = 0;
                if (is_proc) func.mod = FUNC_PROC;
                else func.mod = FUNC_DYNAMIC;
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
                if (decl_type != DECL_VAL) {
                    error(1, 0, "expected '=' or ':=', got '('");
                }
                sxx var_id = lookup_ident(varname);
                if (var_id == -1) {
                    func_statement_op = lookup_intrinsic(varname);
                } else {
                    func_statement_op = OP_FUNC;
                }
                if (var_id == -1 && func_statement_op == OP_NULL) {
                    error(1, 0, "unknown function name %s", cstr(varname));
                }
                func_statement_var_id = var_id;
                is_func_statement = true;
                parse_state = MODE_EXPR;
                break;
            }
            if (token.id != TOK_DEFINE && token.id != '=') {
                error(1, 0, "expected '=' or ':=', got \"%s\"", cstr(token.substr));
            }
            if (token.id == '=' && decl_type == DECL_VAL) {
                sxx id = lookup_ident(varname);
                if (id == -1) {
                    error(1, 0, "undefined identifier '%s'", cstr(varname));
                }
                if (static_vars[id].decl_type != DECL_VAR) {
                    error(1, 0, "tried to write to constant '%s'", cstr(varname));
                }
                set_ref(id, &target);
            } else if (decl_type == DECL_WRITE) {
                if (token.id != '=') {
                    error(1, 0, "cannot write to pointer using ':=', use '=' instead.");
                }
                sxx id = lookup_ident(varname);
                if (id == -1) {
                    error(1, 0, "undefined identifier '%s'", cstr(varname));
                }
                if (static_vars[id].decl_type == DECL_VAR) {
                    error(1, 0, "writing to variable pointers is not yet implemented, save the variable to a constant first.");
                }
                set_ref_write(id, &target);
            } else {
                uxx id;
                bool deref_target = decl_type == DECL_LOCAL;
                if (interpretor_state == MODE_BUILD_FUNC) {
                    id = global_var_count + local_var_count;
                    target.id = local_var_count;
                    target.mode = deref_target ? ARG_DEREF : ARG_VAL;
                    local_var_count++;
                } else {
                    id = global_var_count;
                    target.id = global_var_count;
                    /* global scope, not to be confused with global lifetime */
                    target.mode = deref_target ? ARG_GLOBAL_DEREF : ARG_GLOBAL_VAL;
                    global_var_count++;
                }
                static_vars[id].name = varname;
                if (decl_type == DECL_LOCAL) {
                    static_vars[id].val = (u64)(&mem_stack[mem_stack_count]);
                    /* FIXME what happens when we have locals inside a func? */
                    mem_stack_count++;
                } else {
                    static_vars[id].val = 0;
                }
                static_vars[id].decl_type = decl_type;
            }
            parse_state = MODE_EXPR;
            break;

        case MODE_EXPR:
            if (rhs.arg.mode != ARG_NULL) {
                error(1, 0, "parse state error");
            }
            stream = read_token(stream, &token);
            if (is_func_statement && token.id == ')'
                && op_stack.lhs_count == 0)
            {
                parse_state = MODE_EXPR_FLUSH_BRACKET;
                break;
            }
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
                /* TODO make this lower precedence than multiplication */
                op_stack_push_unary(&op_stack, OP_NEG, PRECEDENCE_UNARY);
                break;
            case '~':
                /* TODO make this lower precedence than bitwise and */
                op_stack_push_unary(&op_stack, OP_NOT, PRECEDENCE_UNARY);
                break;
            case '+':
                /* this no-op is nice for emphasising sign conventions */
                break;
            case '*':
                op_stack_push_unary(&op_stack, OP_DEREF, PRECEDENCE_UNARY);
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
            } else if (token.id == ',' && is_func_statement) {
                parse_state = MODE_EXPR_FLUSH_COMMA;
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
            if (op_stack.lhs_count == 0 && !is_func_statement) {
                error(1, 0, "Excess close brackets");
            }

            if (op_stack_must_copy(&op_stack, rhs.arg)) {
                op_stack_copy(&op_stack, &rhs.arg, &instruction);
            } else if (op_stack.lhs_count == 0) {
                stream = read_token(stream, &token);
                if (token.id != ';') {
                    error(1, 0, "expected ';', got \"%s\"", cstr(token.substr));
                }
                rhs = (struct partial_instruction){};
                /* TODO: type check that the number of arguments is correct.
                   Requires function pointer types if we want non-constant
                   function application */
                if (func_statement_op == OP_FUNC) {
                    set_ref(func_statement_var_id, &instruction.arg1);
                    instruction.arg2.mode = ARG_STACK_OFFSET;
                    instruction.arg2.id = local_var_count;
                } else {
                    instruction.arg1.mode = ARG_STACK_OFFSET;
                    instruction.arg1.id = local_var_count;
                    instruction.arg2.mode = ARG_CONST;
                    instruction.arg2.id = 0;
                }
                instruction.opcode = func_statement_op;
                parse_state = MODE_INIT;
                is_func_statement = false;
                func_statement_var_id = -1;
                op_stack.temp_var_count = 0;
            } else if (op_stack_can_pop_bracket(&op_stack)) {
                op_stack_pop_bracket(&op_stack, close_token);
                parse_state = MODE_OPERATOR;
            } else {
                op_stack_pop(&op_stack, &rhs.arg, &instruction);
            }
            break;
        case MODE_EXPR_FLUSH_COMMA:
            if (op_stack_must_copy(&op_stack, rhs.arg)) {
                op_stack_copy(&op_stack, &rhs.arg, &instruction);
            } else if (op_stack.lhs_count != 0) {
                op_stack_pop(&op_stack, &rhs.arg, &instruction);
            } else {
                rhs = (struct partial_instruction){};
                parse_state = MODE_EXPR;
            }
            break;
        case MODE_EXPR_FLUSH_FINAL:
            if (is_func_statement) {
                error(1, 0, "unclosed parenthesis in function application");
            }
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

    run(input.data);

    if (argc > 2) {
        output = start_elf(argv[2]);
        compile(output);
        end_elf(output);
    }

    for (uxx i = 0; i < global_var_count; i++) {
        str varname = static_vars[i].name;
        char *cstr = strndup(varname.data, varname.size);
        switch(static_vars[i].decl_type) {
        case DECL_VAL:
            printf("%s := %ld\n", cstr, static_vars[i].val);
            break;
        case DECL_VAR:
            printf("var %s = %ld\n", cstr, static_vars[i].val);
            break;
        case DECL_LOCAL:
            printf("local %s = %ld\n", cstr, *lookup(static_vars[i].val));
            break;
        }
    }

    return 0;
}

