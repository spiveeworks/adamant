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

typedef enum {
    /* 0 - 255 represent their ascii equivalents */
    TOK_IDENT = 256,
    TOK_NUM,
    TOK_DEFINE,
    TOK_FUNC
} token_id;

struct keywords {
    token_id id;
    char *word;
} keywords[1] = {
    {TOK_FUNC, "func"}
};

struct compound_operators {
    token_id id;
    char *word;
} compound_operators[1] = {
    {TOK_DEFINE, ":="}
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
        for (int i = 0; i < ARRAY_SIZE(keywords); i++) {
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
        for (s32 i = 0; i < ARRAY_SIZE(compound_operators); i++) {
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

    char *stream = input.data;
    struct token token;
    while (true) {
        stream = read_token(stream, &token);
        switch ((uxx)token.id) {
        case TOK_IDENT:
            putc('`', stdout);
            for (s32 i = 0; i < token.substr.size; i++) {
                putc(token.substr.data[i], stdout);
            }
            putc('`', stdout);
            putc('\n', stdout);
            break;
        case TOK_NUM:
            printf("%ld\n", token.val);
            break;
        case TOK_DEFINE:
            printf(":=\n");
            break;
        case TOK_FUNC:
            printf("func\n");
            break;
        case '\0':
            return 0;
        default:
            if (IS_PRINTABLE(token.id)) {
                printf("\'%c\'\n", token.id);
            } else {
                printf("%d\n", token.id);
            }
        }
    }
    fflush(stdout);
}
