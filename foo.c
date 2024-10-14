#include <ctype.h>
#include <stdarg.h>
#include <stdint.h>
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
    int64_t cur_tok_num;
};

void
read_token(struct lexer *lexer)
{
    char *endptr;
    const char *program_end = lexer->program + lexer->program_length;

    while (isspace(*lexer->ptr) && lexer->ptr < program_end) {
        lexer->ptr++;
    }

    if (lexer->ptr >= program_end) {
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

    while (!isspace(*lexer->ptr) && *lexer->ptr != '(' && *lexer->ptr != ')' && lexer->ptr < program_end) {
        lexer->ptr++;
    }

    lexer->cur_tok_len = lexer->ptr - lexer->cur_tok;

    lexer->cur_tok_num = strtoll(lexer->cur_tok, &endptr, 10);
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

        int64_t number;
    };
};

struct reader {
    struct lexer *lexer;
    struct value value;

    int n_interned;
    char **interned_name;
    int *interned_name_len;
    char **interned_mangled;
    int *interned_mangled_len;
};

void read_value(struct reader *reader);

char *
mangle_name(const char *name, int name_len)
{
    char buf[1024];
    char *dst = buf;
    const char *src = name;
    char num_buf[16];
    int num_len;
    int mangled_len;
    char *ret_buf;

    while (src < name + name_len) {
        if (isalnum(*src)) {
            if (dst - buf >= 1) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            *dst++ = *src++;
        } else if (*src == '_') {
            if (dst - buf >= 2) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            *dst++ = '_';
            *dst++ = '_';
        } else if (*src == '-') {
            if (dst - buf >= 1) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            *dst = '_';
        } else {
            sprintf(num_buf, "%d", *src);
            num_len = strlen(num_buf);
            if (dst - buf > num_len) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            memcpy(dst, num_buf, num_len);
            dst += num_len;
        }
    }

    if (dst - buf >= 1) {
        fprintf(stderr, "name too long: %.*s\n", name_len, name);
        exit(1);
    }

    *dst = 0;

    mangled_len = strlen(buf);
    ret_buf = malloc(mangled_len);
    memcpy(ret_buf, buf, mangled_len + 1);

    return ret_buf;
}

int
intern_name(struct reader *reader, const char *name, int name_len)
{
    int i;

    for (i = 0; i < reader->n_interned; ++i) {
        if (name_len == reader->interned_name_len[i] &&
            memcmp(reader->interned_name[name_len], name, name_len) == 0)
        {
            return i;
        }
    }

    reader->n_interned++;
    reader->interned_name = realloc(reader->interned_name, sizeof(const char *) * reader->n_interned);
    reader->interned_name_len = realloc(reader->interned_name_len, sizeof(const char *) * reader->n_interned);
    reader->interned_mangled = realloc(reader->interned_mangled, sizeof(const char *) * reader->n_interned);
    reader->interned_mangled_len = realloc(reader->interned_mangled_len, sizeof(const char *) * reader->n_interned);

    i = reader->n_interned - 1;
    reader->interned_name_len[i] = name_len;
    reader->interned_name[i] = malloc(name_len);
    memcpy(reader->interned_name[i], name, name_len);

    reader->interned_mangled[i] = mangle_name(name, name_len);
    reader->interned_mangled_len[i] = strlen(reader->interned_mangled[i]);

    return i;
}

void read_list(struct reader *reader)
{
    int reading_tail = 0;
    int read_tail = 0;
    struct value *list_ptr = NULL;
    struct value *list_tail = NULL;
    int list_len = 0;

    for (read_token(reader->lexer);
         reader->lexer->cur_tok_type != TOK_RPAR && reader->lexer->cur_tok_type != TOK_EOF;)
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
        intern_name(reader, reader->lexer->cur_tok, reader->lexer->cur_tok_len);
        break;
    case TOK_EOF:
        fprintf(stderr, "internal error: read_value called on EOF\n");
        exit(1);
    default:
        fprintf(stderr, "read error\n");
        exit(1);
    }

    read_token(reader->lexer);
}

/*********************** compiler ************************/

struct compiler
{
    struct reader *reader;
    const char *output_filename;
    FILE *output_file;

    int varnum;
};

int compile_form(struct compiler *compiler, struct value *form);

void
gen_code(struct compiler *compiler, const char *fmt, ...)
{
    va_list args;
    char buf[256];

    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    fprintf(compiler->output_file, "%s", buf);
}

int
compile_number(struct compiler *compiler, struct value *form)
{
    int varnum = compiler->varnum++;
    gen_code(compiler, "value x%d = fixnum(%lld);\n", varnum, form->number);
    return varnum;
}

int
compile_identifier(struct compiler *compiler, struct value *form)
{
    /*
    int varnum = compiler->varnum++;
    const char *mangled_name = mangle_name(form->identifier.name, form->identifier.name_len);
    gen_code(compiler, "value X%d = %s;\n", varnum, mangled_name);
    free(mangled_name);
    return varnum;
    */

    return 12345;
}

int compile_car(struct compiler *compiler, struct value *form)
{
    if (form->list.length != 2) {
        fprintf(stderr, "car expects a single argument\n");
        exit(1);
    }

    int arg_varnum = compile_form(compiler, &form->list.ptr[0]);
    int dst_varnum = compiler->varnum++;
    gen_code(compiler, "value x%d = car(x%d);", dst_varnum, arg_varnum);
    return dst_varnum;
}

int compile_add(struct compiler *compiler, struct value *form)
{
    int dst_varnum;
    int arg_varnum;

    if (form->list.length == 1) {
        dst_varnum = compiler->varnum++;
        gen_code(compiler, "value x%d = 0;\n", dst_varnum);
    }

    arg_varnum = compile_form(compiler, &form->list.ptr[1]);
    dst_varnum = arg_varnum;
    for (int i = 2; i < form->list.length; ++i) {
        dst_varnum = compile_form(compiler, &form->list.ptr[i]);
        gen_code(compiler, "x%d += (int64_t) x%d;\n", dst_varnum, arg_varnum);
        arg_varnum = dst_varnum;
    }

    return dst_varnum;
}

int
compile_list(struct compiler *compiler, struct value *form)
{
    int varnum;

    if (form->list.length == 0) {
        fprintf(stderr, "the empty list is not a valid form\n");
        exit(1);
    }

    struct value *list_car = &form->list.ptr[0];
    if (list_car->type == VAL_ID &&
        list_car->identifier.name_len == 3 &&
        memcmp(list_car->identifier.name, "car", 3) == 0)
    {
        varnum = compile_car(compiler, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 1 &&
               list_car->identifier.name[0] == '+')
    {
        varnum = compile_add(compiler, form);
    }

    return varnum;
 }

int
compile_form(struct compiler *compiler, struct value *form)
{
    int varnum;
    if (form->type == VAL_NUM) {
        varnum = compile_number(compiler, form);
    } else if (form->type == VAL_ID) {
        varnum = compile_identifier(compiler, form);
    } else if (form->type == VAL_LIST) {
        varnum = compile_list(compiler, form);
    } else {
        fprintf(stderr, "unhandled value type\n");
        exit(1);
    }

    return varnum;
}

void
compile_program(struct compiler *compiler)
{
    FILE *fp = fopen(compiler->output_filename, "w");
    compiler->output_file = fp;
    fprintf(fp, "#include <stdint.h>\n");
    fprintf(fp, "\n");
    fprintf(fp, "typedef void* value;\n");
    fprintf(fp, "typedef void(*kont)(value v);\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define fixnum(v) (value)((int64_t)(v) << 3)");
    fprintf(fp, "\n");
    fprintf(fp, "int main(int argc, const char *argv[]) {\n");

    while (compiler->reader->lexer->cur_tok_type != TOK_EOF) {
        read_value(compiler->reader);
        struct value *form = &compiler->reader->value;
        compile_form(compiler, form);
    }

    fprintf(fp, "}\n");
    fclose(fp);
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
    struct compiler compiler = {
        .reader = &reader,
        .output_filename = "a.c",
    };
    compile_program(&compiler);

    return 0;
}
