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

/*********************** reader ************************/

enum value_type
{
    VAL_LIST,
    VAL_ID,
    VAL_NUM,
};

struct value
{
    enum value_type type;

    int tok_start_idx;
    int tok_len;

    union {
        struct {
            struct value *ptr;
            int length;
            struct value *tail;
        } list;

        struct {
            const char *name;
            int name_len;
        } identifier;

        long number;
    };
};

struct reader {
    struct lexer *lexer;
    struct value value;
};

void read_value(struct reader *reader);

void read_list(struct reader *reader)
{
    int reading_tail = 0;
    int read_tail = 0;
    struct value *list_ptr = NULL;
    struct value *list_tail = NULL;
    int list_len = 0;

    for (read_token(reader->lexer);
         reader->lexer->cur_tok_type != TOK_RPAR && reader->lexer->cur_tok_type != TOK_EOF;
         read_token(reader->lexer))
    {
        read_value(reader);

        if (read_tail) {
            fprintf(stderr, "read error: more than one item after dot\n");
            exit(1);
        } else if (reading_tail) {
            list_tail = malloc(sizeof(struct value));
            memcpy(list_tail, &reader->value, sizeof(struct value));
            read_tail = 1;
            continue;
        } else if (reader->lexer->cur_tok_type == TOK_ID && reader->lexer->cur_tok_len == 1 && reader->lexer->cur_tok[0] == '.') {
            reading_tail = 1;
            continue;
        }

        list_len++;
        list_ptr = realloc(list_ptr, list_len * sizeof(struct value));
        memcpy(list_ptr + list_len - 1, &reader->value, sizeof(struct value));
    }

    if (reader->lexer->cur_tok_type != TOK_RPAR) {
        fprintf(stderr, "read error: EOF in the middle of a list\n");
        exit(1);
    }

    reader->value.type = VAL_LIST;
    reader->value.list.ptr = list_ptr;
    reader->value.list.length = list_len;
    reader->value.list.tail = list_tail;

    /* read past the RPAR */
    read_token(reader->lexer);
}

void
read_value(struct reader *reader)
{
    /* this function expects read_token to have been called already */

    reader->value.tok_start_idx = reader->lexer->cur_tok - reader->lexer->program;
    reader->value.tok_len = reader->lexer->cur_tok_len;

    switch (reader->lexer->cur_tok_type)
    {
    case TOK_LPAR:
        read_list(reader);
        break;
    case TOK_NUM:
        reader->value.type = VAL_NUM;
        reader->value.number = reader->lexer->cur_tok_num;
        break;
    case TOK_ID:
        reader->value.type = VAL_ID;
        reader->value.identifier.name = reader->lexer->cur_tok;
        reader->value.identifier.name_len = reader->lexer->cur_tok_len;
        break;
    case TOK_EOF:
        fprintf(stderr, "internal error: read_value called on EOF\n");
        exit(1);
    default:
        fprintf(stderr, "read error\n");
        exit(1);
    }

}

/*********************** printer ************************/

void
print_value(struct value *value)
{
    switch (value->type) {
    case VAL_NUM:
        fprintf(stderr, "<number %d>", value->number);
        break;
    case VAL_ID:
        fprintf(stderr, "<id %.*s>", value->identifier.name_len, value->identifier.name);
        break;
    case VAL_LIST:
        fprintf(stderr, "<list (");

        for (int i = 0; i < value->list.length; ++i) {
            print_value(value->list.ptr + i);
            if (i < value->list.length - 1) {
                fprintf(stderr, " ");
            }
        }

        if (value->list.tail) {
            fprintf(stderr, " . ");
            print_value(value->list.tail);
        }

        fprintf(stderr, ")>");
        break;
    default:
        fprintf(stderr, "<unknown value>");
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

    struct reader reader = {
        .lexer = &lexer,
    };

    read_token(&lexer);
    while (lexer.cur_tok_type != TOK_EOF) {
        read_value(&reader);
        print_value(&reader.value);
        fprintf(stderr, "\n");
    }

    return 0;
}
