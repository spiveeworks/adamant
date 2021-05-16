#include <stdio.h>
#include <error.h>
#include <errno.h>

int main(int argc, char **argv) {
    FILE *input = NULL;
    FILE *output = NULL;

    if (argc == 1) {
        error(1, 0, "expected input file");
    }
    if (argc > 3) {
        error(1, 0, "too many arguments");
    }

    input = fopen(argv[1], "r");
    if (!input) {
        error(1, errno, "error opening input file");
    }

    if (argc > 2) {
        output = fopen(argv[2], "w");
        if (!output) {
            error(1, errno, "error opening output file");
        }
    }
}
