#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*********************** utils ************************/
const char *
read_file(const char *filename, long *length)
{
    char *buffer;
    FILE *fp = fopen(filename, "rb");
    fseek(fp, 0, SEEK_END);
    *length = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    buffer = malloc(*length + 1);
    fread(buffer, 1, *length, fp);
    buffer[*length] = 0;
    return buffer;
}

/*********************** lexer ************************/
enum token_type
{
    TOK_LPAR,
    TOK_RPAR,
    TOK_NUM,
    TOK_ID,
    TOK_EOF,
};

struct lexer
{
    const char *filename;
    const char *program;
    long program_length;
    const char *ptr;

    enum token_type cur_tok_type;
    const char *cur_tok;
    int cur_tok_len;
    long cur_tok_num;
};

void
read_token(struct lexer *lexer)
{
    char *endptr;

    if (lexer->ptr >= lexer->program + lexer->program_length) {
        lexer->cur_tok_type = TOK_EOF;
        return;
    }

    while (isspace(*lexer->ptr)) {
        lexer->ptr++;
    }

    if (lexer->ptr >= lexer->program + lexer->program_length) {
        lexer->cur_tok_type = TOK_EOF;
        return;
    }

    lexer->cur_tok = lexer->ptr;

    if (*lexer->ptr == '(') {
        lexer->cur_tok_len = 1;
        lexer->cur_tok_type = TOK_LPAR;
        lexer->ptr++;
        return;
    }

    if (*lexer->ptr == ')') {
        lexer->cur_tok_len = 1;
        lexer->cur_tok_type = TOK_RPAR;
        lexer->ptr++;
        return;
    }

    while (!isspace(*lexer->ptr) && *lexer->ptr != '(' && *lexer->ptr != ')') {
        lexer->ptr++;
    }

    lexer->cur_tok_len = lexer->ptr - lexer->cur_tok;

    lexer->cur_tok_num = strtol(lexer->cur_tok, &endptr, 10);
    if (endptr == lexer->ptr) {
        lexer->cur_tok_type = TOK_NUM;
    } else {
        lexer->cur_tok_type = TOK_ID;
    }
}

/*********************** main ************************/

int
main(int argc, char const *argv[])
{
    const char *filename;
    const char *program;
    long program_length;

    if (argc != 2) {
        fprintf(stderr, "Usage: %s input_file\n", argv[0]);
        return 1;
    }

    filename = argv[1];
    program = read_file(filename, &program_length);
    struct lexer lexer = {
        .filename = filename,
        .program = program,
        .program_length = program_length,
        .ptr = program,
    };

    for (read_token(&lexer); lexer.cur_tok_type != TOK_EOF; read_token(&lexer)) {
        const char *type_str;
        switch (lexer.cur_tok_type) {
        case TOK_LPAR:
            type_str = "LPAR";
            break;
        case TOK_RPAR:
            type_str = "RPAR";
            break;
        case TOK_NUM:
            type_str = "NUM";
            break;
        case TOK_ID:
            type_str = "ID";
            break;
        case TOK_EOF:
            type_str = "EOF";
            break;
        }

        printf("%s %.*s\n", type_str, lexer.cur_tok_len, lexer.cur_tok);
    }

    return 0;
}
