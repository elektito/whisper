#include <argp.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*********************** utils ************************/
const char *
read_file(const char *filename, long *length)
{
    char *buffer;
    FILE *fp = fopen(filename, "rb");
    if (fp == NULL) {
        perror("error opening file");
        exit(1);
    }

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
    TOK_QUOTE,
    TOK_NUM,
    TOK_STR,
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

    do {
        while (isspace(*lexer->ptr) && lexer->ptr < program_end) {
            lexer->ptr++;
        }

        if (lexer->ptr >= program_end) {
            lexer->cur_tok_type = TOK_EOF;
            return;
        }

        if (*lexer->ptr == ';') {
            while (*lexer->ptr != '\n' && lexer->ptr < program_end) {
                lexer->ptr++;
            }
        }

        if (lexer->ptr >= program_end) {
            lexer->cur_tok_type = TOK_EOF;
            return;
        }
    } while (isspace(*lexer->ptr));

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

    if (*lexer->ptr == '\'') {
        lexer->cur_tok_len = 1;
        lexer->cur_tok_type = TOK_QUOTE;
        lexer->ptr++;
        return;
    }

    if (*lexer->ptr == '"') {
        lexer->cur_tok++;
        lexer->ptr++;

        while (*lexer->ptr != '"' && lexer->ptr < program_end) {
            lexer->ptr++;
        }

        if (*lexer->ptr != '"') {
            fprintf(stderr, "eof in the middle of string\n");
            exit(1);
        }

        lexer->cur_tok_len = lexer->ptr - lexer->cur_tok;
        lexer->cur_tok_type = TOK_STR;
        lexer->ptr++;
        return;
    }

    if (*lexer->ptr == '#') {
        lexer->ptr++;
        if (lexer->ptr >= program_end) {
            fprintf(stderr, "eof after sharp\n");
            exit(1);
        }

        if (*lexer->ptr == '\\') {
            /* make sure we read at least one character after */
            lexer->ptr++;

            if (lexer->ptr >= program_end) {
                fprintf(stderr, "eof in character literal\n");
                exit(1);
            }

            /* read the next possible character */
            lexer->ptr++;
        }
    }

    while (!isspace(*lexer->ptr) &&
           *lexer->ptr != '(' &&
           *lexer->ptr != ')' &&
           *lexer->ptr != '\'' &&
           lexer->ptr < program_end)
    {
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

/* interned_string is used when we hold an index into the
 * interned_* arrays in struct reader. */
typedef int interned_string;

enum value_type
{
    VAL_LIST,
    VAL_ID,
    VAL_NUM,
    VAL_STR,
    VAL_BOOL,
    VAL_CHAR,
    VAL_EOF,
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
            interned_string interned;
        } identifier;

        struct {
            const char *ptr;
            int length;
        } string;

        int64_t number;
        int bool;
        char character;
    };
};

struct reader {
    struct lexer *lexer;
    struct value value;

    int n_lexers;
    struct lexer **lexers;

    int n_interned;
    char **interned_name;
    int *interned_name_len;
    char **interned_mangled;
    int *interned_mangled_len;

    /* known identifiers */
    interned_string id_true;
    interned_string id_false;
    interned_string id_char_alarm;
    interned_string id_char_backspace;
    interned_string id_char_delete;
    interned_string id_char_escape;
    interned_string id_char_newline;
    interned_string id_char_null;
    interned_string id_char_return;
    interned_string id_char_space;
    interned_string id_char_tab;
};

void read_value(struct reader *reader);
int intern_name(struct reader *reader, const char *name, int name_len);

struct reader *
create_reader(struct lexer *lexer)
{
    struct reader *reader = calloc(sizeof(struct reader), 1);

    reader->lexer = lexer;

    reader->n_lexers = 1;
    reader->lexers = malloc(sizeof(struct lexer *));
    reader->lexers[0] = lexer;

    reader->id_true = intern_name(reader, "#t", 2);
    reader->id_false = intern_name(reader, "#f", 2);
    reader->id_char_alarm = intern_name(reader, "#\\alarm", 7);
    reader->id_char_backspace = intern_name(reader, "#\\backspace", 11);
    reader->id_char_delete = intern_name(reader, "#\\delete", 8);
    reader->id_char_escape = intern_name(reader, "#\\escape", 8);
    reader->id_char_newline = intern_name(reader, "#\\newline", 9);
    reader->id_char_null = intern_name(reader, "#\\null", 6);
    reader->id_char_return = intern_name(reader, "#\\return", 8);
    reader->id_char_space = intern_name(reader, "#\\space", 7);
    reader->id_char_tab = intern_name(reader, "#\\tab", 5);

    return reader;
}

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

    *dst++ = '_';

    while (src < name + name_len) {
        if (isalnum(*src)) {
            if (sizeof(buf) - (dst - buf) < 1) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            *dst++ = *src;
        } else if (*src == '_') {
            if (sizeof(buf) - (dst - buf) < 2) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            *dst++ = '_';
            *dst++ = '_';
        } else if (*src == '-') {
            if (sizeof(buf) - (dst - buf) < 1) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            *dst++ = '_';
        } else {
            sprintf(num_buf, "%d", *src);
            num_len = strlen(num_buf);
            if (sizeof(buf) - (dst - buf) < num_len - 1) {
                fprintf(stderr, "name too long: %.*s\n", name_len, name);
                exit(1);
            }

            *dst++ = '_';
            memcpy(dst, num_buf, num_len);
            dst += num_len;
        }

        src++;
    }

    if (sizeof(buf) - (dst - buf) < 1) {
        fprintf(stderr, "name too long: %.*s\n", name_len, name);
        exit(1);
    }

    *dst = 0;

    mangled_len = strlen(buf);
    ret_buf = malloc(mangled_len + 1);
    memcpy(ret_buf, buf, mangled_len + 1);

    return ret_buf;
}

int
intern_name(struct reader *reader, const char *name, int name_len)
{
    int i;

    for (i = 0; i < reader->n_interned; ++i) {
        if (name_len == reader->interned_name_len[i] &&
            memcmp(reader->interned_name[i], name, name_len) == 0)
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
        } else if (reader->value.type == VAL_ID && reader->value.identifier.name_len == 1 && reader->value.identifier.name[0] == '.') {
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
read_quoted_value(struct reader *reader)
{
    read_token(reader->lexer);

    struct value *value = calloc(sizeof(struct value), 1);
    read_value(reader);
    memcpy(value, &reader->value, sizeof(struct value));

    reader->value.type = VAL_LIST;
    reader->value.list.length = 2;
    reader->value.list.tail = NULL;
    reader->value.list.ptr = calloc(sizeof(struct value), 2);

    reader->value.list.ptr[0].type = VAL_ID;
    reader->value.list.ptr[0].identifier.name = "quote";
    reader->value.list.ptr[0].identifier.name_len = 5;

    memcpy(&reader->value.list.ptr[1], value, sizeof(struct value));
    free(value);
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
        read_token(reader->lexer);
        break;
    case TOK_NUM:
        reader->value.type = VAL_NUM;
        reader->value.number = reader->lexer->cur_tok_num;
        read_token(reader->lexer);
        break;
    case TOK_STR:
        reader->value.type = VAL_STR;
        reader->value.string.ptr = reader->lexer->cur_tok;
        reader->value.string.length = reader->lexer->cur_tok_len;
        read_token(reader->lexer);
        break;
    case TOK_QUOTE:
        read_quoted_value(reader);
        break;
    case TOK_ID:
        reader->value.identifier.name = reader->lexer->cur_tok;
        reader->value.identifier.name_len = reader->lexer->cur_tok_len;
        reader->value.identifier.interned = intern_name(reader, reader->lexer->cur_tok, reader->lexer->cur_tok_len);

        if (reader->value.identifier.interned == reader->id_true) {
            reader->value.type = VAL_BOOL;
            reader->value.bool = 1;
        } else if (reader->value.identifier.interned == reader->id_false) {
            reader->value.type = VAL_BOOL;
            reader->value.bool = 0;
        } else if (reader->value.identifier.name_len >= 3 &&
                   reader->value.identifier.name[0] == '#' &&
                   reader->value.identifier.name[1] == '\\')
        {
            reader->value.type = VAL_CHAR;

            if (reader->value.identifier.name_len == 3) {
                reader->value.character = reader->value.identifier.name[2];
            } else if (reader->value.identifier.interned == reader->id_char_alarm) {
                reader->value.character = '\x07';
            } else if (reader->value.identifier.interned == reader->id_char_backspace) {
                reader->value.character = '\b';
            } else if (reader->value.identifier.interned == reader->id_char_delete) {
                reader->value.character = '\x7f';
            } else if (reader->value.identifier.interned == reader->id_char_escape) {
                reader->value.character = '\x1b';
            } else if (reader->value.identifier.interned == reader->id_char_newline) {
                reader->value.character = '\n';
            } else if (reader->value.identifier.interned == reader->id_char_null) {
                reader->value.character = '\0';
            } else if (reader->value.identifier.interned == reader->id_char_return) {
                reader->value.character = '\r';
            } else if (reader->value.identifier.interned == reader->id_char_space) {
                reader->value.character = ' ';
            } else if (reader->value.identifier.interned == reader->id_char_tab) {
                reader->value.character = '\t';
            } else if (reader->value.identifier.name_len == 5 && reader->value.identifier.name[2] == 'x') {
                char first_digit = reader->value.identifier.name[3];
                char second_digit = reader->value.identifier.name[4];
                int first_digit_value, second_digit_value;
                if (first_digit >= '0' && first_digit <= '9') {
                    first_digit_value = first_digit - '0';
                } else if (first_digit >= 'a' && first_digit <= 'z') {
                    first_digit_value = first_digit - 'a' + 10;
                } else if (first_digit >= 'A' && first_digit <= 'Z') {
                    first_digit_value = first_digit - 'A' + 10;
                } else {
                    fprintf(stderr, "invalid character literal\n");
                    exit(1);
                }

                if (second_digit >= '0' && second_digit <= '9') {
                    second_digit_value = second_digit - '0';
                } else if (second_digit >= 'a' && second_digit <= 'z') {
                    second_digit_value = second_digit - 'a' + 10;
                } else if (second_digit >= 'A' && second_digit <= 'Z') {
                    second_digit_value = second_digit - 'A' + 10;
                } else {
                    fprintf(stderr, "invalid character literal\n");
                    exit(1);
                }

                reader->value.character = (char)(first_digit_value * 16 + second_digit_value);
            } else {
                fprintf(stderr, "invalid character literal\n");
                exit(1);
            }
        } else {
            reader->value.type = VAL_ID;
        }

        read_token(reader->lexer);

        break;
    case TOK_EOF:
        if (reader->n_lexers == 1) {
            reader->value.type = VAL_EOF;
            return;
        }

        reader->n_lexers--;
        reader->lexer = reader->lexers[reader->n_lexers - 1];
        reader->lexers = realloc(reader->lexers, sizeof(struct lexer *) * reader->n_lexers);

        read_value(reader);

        break;
    default:
        fprintf(stderr, "read error\n");
        exit(1);
    }
}

/*********************** compiler ************************/

#define INDENT_SIZE 4

struct function
{
    struct compiler *compiler;
    char *name;
    struct function *parent;

    char *code;
    int code_size;

    /* the counter used for naming variable names inside a function */
    int varnum;

    int n_freevars;
    interned_string *freevars;

    int has_rest;
    int n_params;
    interned_string *params;
};

struct compiler
{
    struct reader *reader;
    const char *c_filename;
    FILE *output_file;

    int n_functions;
    struct function **functions;

    int n_symbols;
    interned_string *symbols;

    int n_referenced_vars;
    interned_string *referenced_vars;
};

int compile_form(struct function *func, int indent, struct value *form);

void
add_compiled_symbol(struct compiler *compiler, interned_string identifier)
{
    for (int i = 0; i < compiler->n_symbols; ++i) {
        if (compiler->symbols[i] == identifier) {
            return;
        }
    }

    compiler->n_symbols++;
    compiler->symbols = realloc(compiler->symbols, sizeof(interned_string) * compiler->n_symbols);
    compiler->symbols[compiler->n_symbols - 1] = identifier;
}

void
gen_code(struct function *func, int indent, const char *fmt, ...)
{
    va_list args;
    char buf[2048];
    int len;

    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    len = strlen(buf);
    func->code = realloc(func->code, func->code_size + len + indent * INDENT_SIZE);
    memset(func->code + func->code_size, ' ', indent * INDENT_SIZE);
    memcpy(func->code + func->code_size + indent * INDENT_SIZE, buf, len);
    func->code_size += len + indent * INDENT_SIZE;
}

struct function *
add_function(struct function *parent, struct compiler *compiler, int nparams, int has_rest)
{
    char name_buf[1024];
    int name_len;

    if (has_rest) {
        nparams++;
    }

    struct function *func = calloc(sizeof(struct function), 1);
    func->parent = parent;
    func->compiler = compiler;
    func->code_size = 0;
    func->code = NULL;
    func->params = malloc(nparams * sizeof(interned_string));
    func->n_params = nparams;
    func->has_rest = has_rest;

    snprintf(name_buf, sizeof(name_buf), "f%d", compiler->n_functions++);
    name_len = strlen(name_buf);
    func->name = malloc(name_len + 1);
    memcpy(func->name, name_buf, name_len + 1);

    compiler->functions = realloc(compiler->functions, compiler->n_functions * sizeof(struct function *));
    compiler->functions[compiler->n_functions - 1] = func;

    return func;
}

void
add_referenced_global_var(struct compiler *compiler, interned_string var)
{
    for (int i = 0; i < compiler->n_referenced_vars; ++i) {
        if (compiler->referenced_vars[i] == var) {
            return;
        }
    }

    compiler->n_referenced_vars++;
    compiler->referenced_vars = realloc(compiler->referenced_vars, compiler->n_referenced_vars * sizeof(interned_string));
    compiler->referenced_vars[compiler->n_referenced_vars - 1] = var;
}

int
compile_number(struct function *func, int indent, struct value *form)
{
    int varnum = func->varnum++;
    gen_code(func, indent, "value x%d = FIXNUM(%lld);\n", varnum, form->number);
    return varnum;
}

int
compile_bool(struct function *func, int indent, struct value *form)
{
    int varnum = func->varnum++;
    if (form->bool) {
        gen_code(func, indent, "value x%d = TRUE;\n", varnum);
    } else {
        gen_code(func, indent, "value x%d = FALSE;\n", varnum);
    }

    return varnum;
}

int
compile_primcall_name(struct function *func, int indent, struct value *form);

int
compile_identifier(struct function *func, int indent, struct value *form)
{
    int found = 0;
    int varnum = func->varnum++;

    for (int i = 0; i < func->n_params; ++i) {
        if (func->params[i] == form->identifier.interned) {
            gen_code(func, indent, "value x%d = %.*s;\n",
                     varnum,
                     func->compiler->reader->interned_mangled_len[form->identifier.interned],
                     func->compiler->reader->interned_mangled[form->identifier.interned]);
            found = 1;
            break;
        }
    }

    if (!found) {
        /* check parent params to see if it's a global variable or not */
        struct function *parent = func->parent;
        found = 0;
        while (parent) {
            for (int i = 0; i < parent->n_params; ++i) {
                if (parent->params[i] == form->identifier.interned) {
                    found = 1;
                    break;
                }
            }

            if (found) {
                break;
            }

            parent = parent->parent;
        }

        if (!found || parent->parent == NULL) {
            if (!found) {
                /* if not found in any parent (i.e. not even in
                 * top-level), first check to see if its a primcall
                 * name */
                int primcall_varnum = compile_primcall_name(func, indent, form);
                if (primcall_varnum > -1) {
                    return primcall_varnum;
                }
            }

            /* it's a global variable */
            gen_code(func, indent, "value x%d = %.*s;\n", varnum,
                     func->compiler->reader->interned_mangled_len[form->identifier.interned],
                     func->compiler->reader->interned_mangled[form->identifier.interned]);

            add_referenced_global_var(func->compiler, form->identifier.interned);

            return varnum;
        }

        /* it's a free variable */
        found = 0;
        for (int j = 0; j < func->n_freevars; ++j) {
            if (func->freevars[j] == form->identifier.interned) {
                /* it's been used before */
                gen_code(func, indent, "value x%d = envget(env, %d);\n", varnum, j);
                found = 1;
                break;
            }
        }

        if (!found) {
            func->n_freevars++;
            func->freevars = realloc(func->freevars, func->n_freevars * sizeof(interned_string));
            func->freevars[func->n_freevars - 1] = form->identifier.interned;
            gen_code(func, indent, "value x%d = envget(env, %d);\n", varnum, func->n_freevars - 1);
        }
    }

    return varnum;
}

int
compile_string(struct function *func, int indent, struct value *form)
{
    int varnum = func->varnum++;

    gen_code(func, indent, "value x%d = make_string(\"", varnum);

    for (int i = 0; i < form->string.length; ++i) {
        char c = form->string.ptr[i];
        switch (c) {
        case '\b':
            gen_code(func, 0, "\\b");
            break;
        case '\n':
            gen_code(func, 0, "\\n");
            break;
        case '\r':
            gen_code(func, 0, "\\r");
            break;
        case '\t':
            gen_code(func, 0, "\\t");
            break;
        case '\\':
            gen_code(func, 0, "\\\\");
            break;
        case '\"':
            gen_code(func, 0, "\\\"");
            break;
        case '\0':
            gen_code(func, 0, "\\0");
            break;
        default:
            if (c >= 32 && c < 127) {
                /* printable */
                gen_code(func, 0, "%c", c);
            } else {
                /* use octal form for everything else */
                gen_code(func, 0, "\\%03o", (unsigned char) c);
            }
        }
    }

    gen_code(func, 0, "\", %d);\n", form->string.length);

    return varnum;
}

int
compile_char(struct function *func, int indent, struct value *form)
{
    int varnum = func->varnum++;
    char c = form->character;

    gen_code(func, indent, "value x%d = CHAR('", varnum);

    switch (c) {
    case '\b':
        gen_code(func, 0, "\\b");
        break;
    case '\n':
        gen_code(func, 0, "\\n");
        break;
    case '\r':
        gen_code(func, 0, "\\r");
        break;
    case '\t':
        gen_code(func, 0, "\\t");
        break;
    case '\\':
        gen_code(func, 0, "\\\\");
        break;
    case '\'':
        gen_code(func, 0, "\\\'");
        break;
    case '\0':
        gen_code(func, 0, "\\0");
        break;
    default:
        if (c >= 32 && c < 127) {
            gen_code(func, 0, "%c", c);
        } else {
            gen_code(func, 0, "\\%03o", (unsigned char) c);
        }
    }

    gen_code(func, 0, "');\n");
    return varnum;
}

int
compile_function(struct function *func, int indent, struct value *form,
                 int is_define)
{
    int varnum = func->varnum++;

    struct value *params = &form->list.ptr[1];
    if (params->type != VAL_LIST && params->type != VAL_ID) {
        fprintf(stderr, "bad argument list\n");
        exit(1);
    }

    int has_rest = 0;
    if (params->list.tail || params->type == VAL_ID) {
        has_rest = 1;
    }

    struct value *rest_param;
    if (params->type == VAL_ID) {
        rest_param = params;
    } else {
        rest_param = params->list.tail;
    }

    int n_params;
    if (is_define) {
        n_params = params->list.length - 1;
    } else {
        n_params = params->type == VAL_ID ? 0 : params->list.length;
    }

    struct function *new_func = add_function(func, func->compiler, n_params, has_rest);
    if (has_rest) {
        gen_code(new_func, 1, "if (nargs < %d) { RAISE(\"too few arguments for function\"); }\n", n_params);
    } else {
        gen_code(new_func, 1, "if (nargs != %d) { RAISE(\"argument count mismatch\"); }\n", n_params);
    }

    gen_code(new_func, 1, "va_list args;\n");
    gen_code(new_func, 1, "va_start(args, nargs);\n");
    for (int i = 0; i < n_params; ++i) {
        int offset = is_define ? 1 : 0;
        struct value *param = &params->list.ptr[i + offset];
        if (param->type != VAL_ID) {
            fprintf(stderr, "parameter not an identifier\n");
            exit(1);
        }
        new_func->params[i] = param->identifier.interned;

        gen_code(new_func, 1, "value %.*s = va_arg(args, value);\n",
                func->compiler->reader->interned_mangled_len[param->identifier.interned],
                func->compiler->reader->interned_mangled[param->identifier.interned]);
    }

    if (has_rest) {
        new_func->params[n_params] = rest_param->identifier.interned;
    }

    if (has_rest) {
        if (rest_param->type != VAL_ID) {
            fprintf(stderr, "parameter not an identifier\n");
            exit(1);
        }

        interned_string rest_idx = rest_param->identifier.interned;

        gen_code(new_func, 1, "value %.*s = NIL;\n",
                 func->compiler->reader->interned_mangled_len[rest_idx],
                 func->compiler->reader->interned_mangled[rest_idx]);
        gen_code(new_func, 1, "for (int i = 0; i < nargs - %d; ++i) { value v = va_arg(args, value); %.*s = make_pair(v, %.*s); }\n",
                 n_params,
                 func->compiler->reader->interned_mangled_len[rest_idx],
                 func->compiler->reader->interned_mangled[rest_idx],
                 func->compiler->reader->interned_mangled_len[rest_idx],
                 func->compiler->reader->interned_mangled[rest_idx]);
        gen_code(new_func, 1, "%.*s = reverse_list(%.*s, NIL);\n",
                 func->compiler->reader->interned_mangled_len[rest_idx],
                 func->compiler->reader->interned_mangled[rest_idx],
                 func->compiler->reader->interned_mangled_len[rest_idx],
                 func->compiler->reader->interned_mangled[rest_idx]);
    }

    gen_code(new_func, 1, "va_end(args);\n");
    gen_code(new_func, 0, "\n");

    int new_varnum = -1;
    for (int i = 2; i < form->list.length; ++i) {
        new_varnum = compile_form(new_func, 1, &form->list.ptr[i]);
    }

    gen_code(new_func, 1, "return x%d;\n", new_varnum);

    /* now generate to code for referencing the function */
    gen_code(func, indent, "value x%d = make_closure(%s, %d, %d",
             varnum,
             new_func->name,
             n_params,
             new_func->n_freevars);
    for (int i = 0; i < new_func->n_freevars; ++i) {
        char buf[64];
        char *fvar = NULL;
        int fvar_len;

        for (int j = 0; j < func->n_params; ++j) {
            if (new_func->freevars[i] == func->params[j]) {
                fvar = func->compiler->reader->interned_mangled[func->params[j]];
                fvar_len = func->compiler->reader->interned_mangled_len[func->params[j]];
            }
        }

        if (fvar == NULL) {
            /* not one of the function's parameters; it's a free
             * variable here too. see if it's already been
             * referenced. */
            for (int j = 0; j < func->n_freevars; ++j) {
                if (new_func->freevars[i] == func->freevars[j]) {
                    snprintf(buf, sizeof(buf), "envget(env, %d)", j);
                    fvar = buf;
                    fvar_len = strlen(buf);
                }
            }
        }

        if (fvar == NULL) {
            /* the free variable has not been referenced before; add it
             * to the list of free variables. */
            func->n_freevars++;
            func->freevars = realloc(func->freevars, func->n_freevars);
            func->freevars[func->n_freevars - 1] = new_func->freevars[i];

            snprintf(buf, sizeof(buf), "envget(env, %d)", func->n_freevars - 1);
            fvar = buf;
            fvar_len = strlen(buf);
        }

        gen_code(func, 0, ", %.*s", fvar_len, fvar);
    }
    gen_code(func, 0, ");\n");

    return varnum;
}

int
compile_lambda(struct function *func, int indent, struct value *form)
{
    if (form->list.length < 3) {
        fprintf(stderr, "invalid lambda form\n");
        exit(1);
    }

    return compile_function(func, indent, form, 0);
}

int
compile_let(struct function *func, int indent, struct value *form)
{
    if (form->list.length < 3) {
        fprintf(stderr, "malformed let\n");
        exit(1);
    }

    int bindings_idx = 1;
    interned_string self_ref_identifier = -1;
    if (form->list.ptr[bindings_idx].type == VAL_ID) {
        self_ref_identifier = form->list.ptr[bindings_idx].identifier.interned;
        bindings_idx = 2;
    }

    struct value *bindings = &form->list.ptr[bindings_idx];
    if (bindings->type != VAL_LIST) {
        fprintf(stderr, "malformed let\n");
        exit(1);
    }

    int n_params = bindings->list.length;
    struct function *new_func = add_function(func, func->compiler, n_params, 0);

    gen_code(new_func, 1, "va_list args;\n");
    gen_code(new_func, 1, "va_start(args, nargs);\n");

    /* add binding variables as parameters */
    for (int i = 0; i < form->list.ptr[bindings_idx].list.length; ++i) {
        struct value *binding = &bindings->list.ptr[i];
        if (binding->type != VAL_LIST || binding->list.length != 2) {
            fprintf(stderr, "bad let binding\n");
            exit(1);
        }

        if (binding->list.ptr[0].type != VAL_ID) {
            fprintf(stderr, "bad let variable\n");
            exit(1);
        }

        new_func->params[i] = binding->list.ptr[0].identifier.interned;
        gen_code(new_func, 1, "value %.*s = va_arg(args, value);\n",
                 func->compiler->reader->interned_mangled_len[binding->list.ptr[0].identifier.interned],
                 func->compiler->reader->interned_mangled[binding->list.ptr[0].identifier.interned]);
    }

    gen_code(new_func, 1, "va_end(args);\n");
    gen_code(new_func, 0, "\n");

    /* if we have a self-reference variable add a dummy parent function
     * (to be removed later) with a single argument: the self-reference
     * variable. this way, referencing the let variable will be
     * considered accessing a free variable, and not a global variable
     * access. */
    struct function *dummy_parent = NULL;
    if (self_ref_identifier != -1) {
        dummy_parent = calloc(1, sizeof(struct function) + sizeof(interned_string) * 1);
        dummy_parent->name = "dummy";
        dummy_parent->n_params = 1;
        dummy_parent->params = malloc(1 * sizeof(interned_string));
        dummy_parent->params[0] = self_ref_identifier;
        dummy_parent->parent = new_func->parent;
        new_func->parent = dummy_parent;
    }

    /* compile body */
    int new_ret_varnum;
    for (int i = bindings_idx + 1; i < form->list.length; ++i) {
        new_ret_varnum = compile_form(new_func, 1, &form->list.ptr[i]);
    }

    gen_code(new_func, 1, "return x%d;\n", new_ret_varnum);

    /* remove dummy parent */
    if (dummy_parent != NULL) {
        new_func->parent = func;
        free(dummy_parent);
    }

    /* now generate the code for referencing the function */
    int func_varnum = func->varnum++;
    gen_code(func, indent, "value x%d = make_closure(%s, %d, %d",
             func_varnum,
             new_func->name,
             bindings->list.length,
             new_func->n_freevars);

    /* resolve free variables (including self-reference) */
    int self_ref_env_idx = -1;
    for (int i = 0; i < new_func->n_freevars; ++i) {
        char buf[64];
        char *fvar = NULL;
        int fvar_len;

        /* see if its a self-reference. */
        if (new_func->freevars[i] == self_ref_identifier) {
            /* pass zero for now, since we can't access it yet; we'll set it after the
               closure is created. */
            fvar = "(value) 0";
            fvar_len = 9;
            self_ref_env_idx = i;
        }

        for (int j = 0; j < func->n_params; ++j) {
            if (new_func->freevars[i] == func->params[j]) {
                fvar = func->compiler->reader->interned_mangled[func->params[j]];
                fvar_len = func->compiler->reader->interned_mangled_len[func->params[j]];
            }
        }

        if (fvar == NULL) {
            /* not one of the function's parameters; it's a free
             * variable here too. see if it's already been
             * referenced. */
            for (int j = 0; j < func->n_freevars; ++j) {
                if (new_func->freevars[i] == func->freevars[j]) {
                    snprintf(buf, sizeof(buf), "envget(env, %d)", j);
                    fvar = buf;
                    fvar_len = strlen(buf);
                }
            }
        }

        if (fvar == NULL) {
            /* the free variable has not been referenced before; add it
             * to the list of free variables. */
            func->n_freevars++;
            func->freevars = realloc(func->freevars, func->n_freevars * sizeof(interned_string));
            func->freevars[func->n_freevars - 1] = new_func->freevars[i];

            snprintf(buf, sizeof(buf), "envget(env, %d)", func->n_freevars - 1);
            fvar = buf;
            fvar_len = strlen(buf);
        }

        gen_code(func, 0, ", %.*s", fvar_len, fvar);
    }

    gen_code(func, 0, ");\n");

    /* now set the self-reference environment variable, if needed. */
    if (self_ref_env_idx >= 0) {
        gen_code(func, indent, "GET_CLOSURE(x%d)->freevars[%d] = x%d;\n", func_varnum, self_ref_env_idx, func_varnum);
    }

    /* now compile the binding values */
    int *arg_varnums = malloc(sizeof(int) * bindings->list.length);
    for (int i = 0; i < bindings->list.length; ++i) {
        arg_varnums[i] = compile_form(func, indent, &bindings->list.ptr[i].list.ptr[1]);
    }

    /* create a call to the function we just created, passing binding values as arguments*/
    int ret_varnum = func->varnum++;
    gen_code(func, indent, "value x%d = GET_CLOSURE(x%d)->func(GET_CLOSURE(x%d)->freevars, %d", ret_varnum, func_varnum, func_varnum, bindings->list.length);
    for (int i = 0; i < bindings->list.length; ++i) {
        gen_code(func, 0, ", x%d", arg_varnums[i]);
    }
    gen_code(func, 0, ");\n");

    free(arg_varnums);

    return ret_varnum;
}

int
compile_call(struct function *func, int indent, struct value *form)
{
    int ret_varnum;
    int func_varnum;
    int *arg_varnums;

    func_varnum = compile_form(func, indent, &form->list.ptr[0]);
    gen_code(func, indent, "if (!IS_CLOSURE(x%d)) { RAISE(\"called object not a procedure\"); }\n", func_varnum);

    arg_varnums = malloc(sizeof(int) * (form->list.length - 1));
    for (int i = 1; i < form->list.length; ++i) {
        arg_varnums[i - 1] = compile_form(func, indent, &form->list.ptr[i]);
    }

    ret_varnum = func->varnum++;
    gen_code(func, indent, "value x%d = GET_CLOSURE(x%d)->func(GET_CLOSURE(x%d)->freevars, %d", ret_varnum, func_varnum, func_varnum, form->list.length - 1);
    for (int i = 0; i < form->list.length - 1; ++i) {
        gen_code(func, 0, ", x%d", arg_varnums[i]);
    }
    gen_code(func, 0, ");\n");

    return ret_varnum;
}

int
compile_define(struct function *func, int indent, struct value *form)
{
    int varnum;

    if (form->list.length < 2) {
        fprintf(stderr, "malformed define\n");
        exit(1);
    }

    if (form->list.ptr[1].type == VAL_ID) {
        int var_name = form->list.ptr[1].identifier.interned;
        int mangled_len = func->compiler->reader->interned_mangled_len[var_name];
        char *mangled_str = func->compiler->reader->interned_mangled[var_name];
        func->n_params++;
        func->params[func->n_params - 1] = var_name;

        if (form->list.length == 2) {
            /* define the variable with a void initial value */
            gen_code(func, indent, "value %.*s = VOID;\n", mangled_len, mangled_str);
            varnum = func->varnum++;
            gen_code(func, indent, "value x%d = %.*s;\n", mangled_len, mangled_str);

            return varnum;
        }

        if (form->list.length != 3) {
            fprintf(stderr, "malformed define\n");
            exit(1);
        }

        /* define variable with initial value */
        varnum = compile_form(func, indent, &form->list.ptr[2]);

        if (func->parent == NULL) {
            /* the variable is only set here, not declared, because it will
             * later be declared as a global variable. */
            gen_code(func, indent, "%.*s = x%d;\n", mangled_len, mangled_str, varnum);
        } else {
            gen_code(func, indent, "value %.*s = x%d;\n", mangled_len, mangled_str, varnum);
        }

        return varnum;
    }

    /* define function */

    if (form->list.ptr[1].type != VAL_LIST ||
        form->list.ptr[1].list.length == 0)
    {
        fprintf(stderr, "malformed define");
        exit(1);
    }

    int var_name = form->list.ptr[1].list.ptr[0].identifier.interned;
    int mangled_len = func->compiler->reader->interned_mangled_len[var_name];
    char *mangled_str = func->compiler->reader->interned_mangled[var_name];
    func->n_params++;
    func->params = realloc(func->params, func->n_params * sizeof(interned_string));
    func->params[func->n_params - 1] = var_name;

    varnum = compile_function(func, indent, form, 1);

    if (func->parent == NULL) {
        /* we are setting a global variable, so no new variable is defined here. */
        gen_code(func, indent, "%.*s = x%d;\n", mangled_len, mangled_str, varnum);
    } else {
        gen_code(func, indent, "value %.*s = x%d;\n", mangled_len, mangled_str, varnum);
    }

    return varnum;
}

int
compile_if(struct function *func, int indent, struct value *form)
{
    if (form->list.length < 3 || form->list.length > 4) {
        fprintf(stderr, "malformed if\n");
        exit(1);
    }

    int cond_varnum = compile_form(func, indent, &form->list.ptr[1]);
    int else_varnum;
    int ret_varnum;
    if (form->list.length == 3) {
        ret_varnum = func->varnum++;
        gen_code(func, indent, "value x%d = VOID;\n", ret_varnum);
        gen_code(func, indent, "if (GET_BOOL(x%d)) {\n", cond_varnum);
        int then_varnum = compile_form(func, indent + 1, &form->list.ptr[2]);
        gen_code(func, indent + 1, "x%d = x%d;\n", ret_varnum, then_varnum);
        gen_code(func, indent, "}\n");
    } else {
        ret_varnum = func->varnum++;
        gen_code(func, indent, "value x%d;\n", ret_varnum);
        gen_code(func, indent, "if (GET_BOOL(x%d)) {\n", cond_varnum);
        int then_varnum = compile_form(func, indent + 1, &form->list.ptr[2]);
        gen_code(func, indent + 1, "x%d = x%d;\n", ret_varnum, then_varnum);
        gen_code(func, indent, "} else {\n");
        else_varnum = compile_form(func, indent + 1, &form->list.ptr[3]);
        gen_code(func, indent + 1, "x%d = x%d;\n", ret_varnum, else_varnum);
        gen_code(func, indent, "}\n");
    }

    return ret_varnum;
}

int
compile_cond(struct function *func, int indent, struct value *form)
{
    int ret_varnum = func->varnum++;
    gen_code(func, indent, "value x%d = VOID;\n", ret_varnum);

    for (int i = 1; i < form->list.length; ++i) {
        struct value *clause = &form->list.ptr[i];
        if (clause->type != VAL_LIST || clause->list.length < 2) {
            fprintf(stderr, "invalid cond clause\n");
            exit(1);
        }

        struct value *condition = &clause->list.ptr[0];
        if (condition->type == VAL_ID &&
            condition->identifier.name_len == 4 &&
            memcmp(condition->identifier.name, "else", 4) == 0)
        {
            if (i != form->list.length - 1) {
                fprintf(stderr, "else not the last cond clause\n");
                exit(1);
            }

            int varnum;
            for (int j = 1; j < clause->list.length; ++j) {
                varnum = compile_form(func, indent + (i - 1), &clause->list.ptr[j]);
            }
            gen_code(func, indent + i - 1, "x%d = x%d;\n", ret_varnum, varnum);
        } else {
            int condition_varnum = compile_form(func, indent + (i - 1), condition);
            gen_code(func, indent + (i - 1), "if (GET_BOOL(x%d)) {\n", condition_varnum);

            int varnum;
            for (int j = 1; j < clause->list.length; ++j) {
                varnum = compile_form(func, indent + i, &clause->list.ptr[j]);
            }
            gen_code(func, indent + i, "x%d = x%d;\n", ret_varnum, varnum);

            gen_code(func, indent + (i - 1), "} else {\n");
        }
    }

    for (int i = 1; i < form->list.length - 1; ++i) {
        gen_code(func, indent + form->list.length - i - 2, "}\n");
    }

    return ret_varnum;
}

int
compile_or(struct function *func, int indent, struct value *form)
{
    int n_values = form->list.length - 1;
    int ret_varnum = func->varnum++;
    gen_code(func, indent, "value x%d;\n", ret_varnum);

    for (int i = 0; i < n_values; ++i) {
        int arg_varnum = compile_form(func, indent + i, &form->list.ptr[i + 1]);
        gen_code(func, indent + i, "if (x%d != FALSE) {\n", arg_varnum);
        gen_code(func, indent + i + 1, "x%d = x%d;\n", ret_varnum, arg_varnum);
        gen_code(func, indent + i, "} else {\n");
    }

    gen_code(func, indent + n_values, "x%d = FALSE;\n", ret_varnum);

    for (int i = n_values - 1; i >= 0; --i) {
        gen_code(func, indent + i, "}\n");
    }

    return ret_varnum;
}

int
compile_begin(struct function *func, int indent, struct value *form)
{
    int ret_varnum = func->varnum++;
    gen_code(func, indent, "value x%d = VOID;\n", ret_varnum);

    for (int i = 1; i < form->list.length; ++i) {
        int arg_varnum = compile_form(func, indent, &form->list.ptr[i]);
        if (i == form->list.length - 1) {
            gen_code(func, indent, "x%d = x%d;\n", ret_varnum, arg_varnum);
        }
    }

    return ret_varnum;
}

int
compile_include(struct function *func, int indent, struct value *form)
{
    if (form->list.length != 2 || form->list.ptr[1].type != VAL_STR) {
        fprintf(stderr, "malformed include\n");
        exit(1);
    }

    char *filename = malloc(form->list.ptr[1].string.length + 1);
    snprintf(filename, sizeof(filename), "%.*s",
             form->list.ptr[1].string.length,
             form->list.ptr[1].string.ptr);

    struct lexer *lexer = calloc(sizeof(struct lexer), 1);
    lexer->filename = filename;

    long program_length;
    const char *program = read_file(filename, &program_length);
    lexer->program = program;
    lexer->program_length = program_length;
    lexer->ptr = program;

    func->compiler->reader->n_lexers++;
    func->compiler->reader->lexers = realloc(func->compiler->reader->lexers, func->compiler->reader->n_lexers * sizeof(struct lexer *));
    func->compiler->reader->lexers[func->compiler->reader->n_lexers - 1] = lexer;

    func->compiler->reader->lexer = lexer;

    /* proceed to first token */
    read_token(lexer);

    return -1;
}

int
compile_error(struct function *func, int indent, struct value *form)
{
    if (form->list.length != 2) {
        fprintf(stderr, "malformed error\n");
        exit(1);
    }

    int msg_varnum = compile_form(func, indent, &form->list.ptr[1]);
    gen_code(func, indent, "printf(\"error: \");\n");
    gen_code(func, indent, "display(x%d);\n", msg_varnum);
    gen_code(func, indent, "printf(\"\\n\");\n");
    gen_code(func, indent, "exit(1);\n");

    int ret_varnum = func->varnum++;
    gen_code(func, indent, "value x%d = VOID;\n", ret_varnum);

    return ret_varnum;
}

int
compile_quoted_item(struct function *func, int indent, struct value *form)
{
    int varnum;
    int car_varnum;

    switch (form->type) {
    case VAL_BOOL:
    case VAL_NUM:
    case VAL_CHAR:
    case VAL_STR:
        varnum = compile_form(func, indent, form);
        break;

    case VAL_ID:
        varnum = func->varnum++;
        add_compiled_symbol(func->compiler, form->identifier.interned);
        gen_code(func, indent, "value x%d = sym%.*s;\n", varnum,
                 func->compiler->reader->interned_mangled_len[form->identifier.interned],
                 func->compiler->reader->interned_mangled[form->identifier.interned]);
        break;

    case VAL_LIST:
        if (form->list.length == 0) {
            varnum = func->varnum++;
            gen_code(func, indent, "value x%d = NIL;\n", varnum);
        } else {
            if (form->list.tail == NULL) {
                varnum = func->varnum++;
                gen_code(func, indent, "value x%d = NIL;\n", varnum);
            } else {
                varnum = compile_quoted_item(func, indent, form->list.tail);
            }

            for (int i = form->list.length - 1; i >= 0; --i) {
                car_varnum = compile_quoted_item(func, indent, &form->list.ptr[i]);
                gen_code(func, indent, "x%d = make_pair(x%d, x%d);\n", varnum, car_varnum, varnum);
            }
        }

        break;

    default:
        fprintf(stderr, "unknown quoted value type\n");
        exit(1);
    }

    return varnum;
}

int
compile_quote(struct function *func, int indent, struct value *form)
{
    if (form->list.length != 2)
    {
        fprintf(stderr, "quote expects a single argument\n");
        exit(1);
    }

    return compile_quoted_item(func, indent, &form->list.ptr[1]);
}

struct {
    const char *name;
    const char *c_name; /* primcall c function name with "primcall_" prefix */
    int min_args;
    int max_args;
} primcalls[] = {
    { "car", "car", 1, 1 },
    { "cdr", "cdr", 1, 1 },
    { "close-port", "close_port", 1, 1 },
    { "cons", "cons", 2, 2 },
    { "current-input-port", "current_input_port", 0, 0 },
    { "current-output-port", "current_output_port", 0, 0 },
    { "current-error-port", "current_error_port", 0, 0 },
    { "display", "display", 1, 2 },
    { "eof-object?", "eof_object_q", 1, 1 },
    { "eq?", "eq_q", 2, 2 },
    { "input-port?", "input_port_q", 1, 1 },
    { "make-string", "make_string", 1, 2 },
    { "number->string", "number_to_string", 1, 2 },
    { "open-input-file", "open_input_file", 1, 1 },
    { "open-output-file", "open_output_file", 1, 1 },
    { "peek-char", "peek_char", 0, 1 },
    { "port?", "port_q", 1, 1 },
    { "read-char", "read_char", 0, 1 },
    { "read-line", "read_line", 0, 1 },
    { "string->number", "string_to_number", 1, 2 },
    { "string->symbol", "string_to_symbol", 1, 1 },
    { "string-append", "string_append", 0, -1 },
    { "string-length", "string_length", 1, 1 },
    { "string-ref", "string_ref", 2, 2 },
    { "string=?", "string_eq_q", 1, -1 },
    { "string?", "string_q", 1, 1 },
    { "symbol?", "symbol_q", 1, 1 },
    { "write", "write", 1, 2 },
    { "write-char", "write_char", 1, 2 },
    { "+", "add", 0, -1 },
    { "-", "sub", 1, -1 },
    { "*", "mul", 0, -1 },
    { "/", "div", 1, -1 },
};

int
compile_primcall(struct function *func, int indent, struct value *form)
{
    for (int i = 0; i < sizeof(primcalls) / sizeof(primcalls[0]); ++i) {
        int len = strlen(primcalls[i].name);
        if (form->list.ptr[0].identifier.name_len == len &&
            memcmp(form->list.ptr[0].identifier.name, primcalls[i].name, len) == 0)
        {
            int nargs = form->list.length - 1;

            if (primcalls[i].min_args == 0 && primcalls[i].max_args == -1) {
                /* no argument count checking is needed */
            } else if (primcalls[i].min_args == primcalls[i].max_args) {
                if (nargs != primcalls[i].min_args) {
                    fprintf(stderr, "%s expects %d argument(s), %d provided.\n", primcalls[i].name, primcalls[i].min_args, nargs);
                    exit(1);
                }
            } else if (primcalls[i].max_args == -1) {
                if (nargs < primcalls[i].min_args) {
                    fprintf(stderr, "too few arguments for %s: at least %d expected, %d provided.\n", primcalls[i].name, primcalls[i].min_args, nargs);
                    exit(1);
                }
            } else {
                if (nargs < primcalls[i].min_args) {
                    fprintf(stderr, "too few arguments for %s: at least %d expected, %d provided.\n", primcalls[i].name, primcalls[i].min_args, nargs);
                    exit(1);
                }

                if (nargs > primcalls[i].max_args) {
                    fprintf(stderr, "too many arguments for %s: at most %d expected, %d provided.\n", primcalls[i].name, primcalls[i].max_args, nargs);
                    exit(1);
                }
            }

            int *arg_varnums = malloc(nargs * sizeof(int));
            for (int i = 0; i < nargs; ++i) {
                arg_varnums[i] = compile_form(func, indent, &form->list.ptr[i + 1]);
            }

            int ret_varnum = func->varnum++;
            gen_code(func, indent, "value x%d = primcall_%s(NULL, %d", ret_varnum, primcalls[i].c_name, nargs);
            for (int i = 0; i < nargs; ++i) {
                gen_code(func, 0, ", x%d", arg_varnums[i]);
            }
            gen_code(func, 0, ");\n");

            free(arg_varnums);

            return ret_varnum;
        }
    }

    return -1;
}

int
compile_primcall_name(struct function *func, int indent, struct value *form)
{
    for (int i = 0; i < sizeof(primcalls) / sizeof(primcalls[0]); ++i) {
        int len = strlen(primcalls[i].name);
        if (form->identifier.name_len == len &&
            memcmp(form->identifier.name, primcalls[i].name, len) == 0)
        {
            int ret_varnum = func->varnum++;
            gen_code(func, indent, "value x%d = make_closure(primcall_%s, 0, 0);", ret_varnum, primcalls[i].name);
            return ret_varnum;
        }
    }

    return -1;
}

int
compile_list(struct function *func, int indent, struct value *form)
{
    int varnum;

    if (form->list.length == 0) {
        fprintf(stderr, "the empty list is not a valid form\n");
        exit(1);
    }

    struct value *list_car = &form->list.ptr[0];
    if (list_car->type == VAL_ID) {
        varnum = compile_primcall(func, indent, form);
        if (varnum >= 0) {
            return varnum;
        }
    }

    if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 5 &&
               memcmp(list_car->identifier.name, "quote", 5) == 0)
    {
        varnum = compile_quote(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 3 &&
               memcmp(list_car->identifier.name, "let", 3) == 0)
    {
        varnum = compile_let(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 6 &&
               memcmp(list_car->identifier.name, "lambda", 6) == 0)
    {
        varnum = compile_lambda(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 6 &&
               memcmp(list_car->identifier.name, "define", 5) == 0)
    {
        varnum = compile_define(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 2 &&
               memcmp(list_car->identifier.name, "if", 2) == 0)
    {
        varnum = compile_if(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 5 &&
               memcmp(list_car->identifier.name, "begin", 5) == 0)
    {
        varnum = compile_begin(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 4 &&
               memcmp(list_car->identifier.name, "cond", 4) == 0)
    {
        varnum = compile_cond(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 2 &&
               memcmp(list_car->identifier.name, "or", 2) == 0)
    {
        varnum = compile_or(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 7 &&
               memcmp(list_car->identifier.name, "include", 7) == 0)
    {
        varnum = compile_include(func, indent, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 5 &&
               memcmp(list_car->identifier.name, "error", 5) == 0)
    {
        varnum = compile_error(func, indent, form);
    } else {
        varnum = compile_call(func, indent, form);
    }

    return varnum;
 }

int
compile_form(struct function *func, int indent, struct value *form)
{
    int varnum;
    if (form->type == VAL_NUM) {
        varnum = compile_number(func, indent, form);
    } else if (form->type == VAL_ID) {
        varnum = compile_identifier(func, indent, form);
    } else if (form->type == VAL_STR) {
        varnum = compile_string(func, indent, form);
    } else if (form->type == VAL_BOOL) {
        varnum = compile_bool(func, indent, form);
    } else if (form->type == VAL_LIST) {
        varnum = compile_list(func, indent, form);
    } else if (form->type == VAL_CHAR) {
        varnum = compile_char(func, indent, form);
    } else if (form->type == VAL_EOF) {
        fprintf(stderr, "internal error: trying to compile EOF\n");
        exit(1);
    } else {
        fprintf(stderr, "unhandled value type\n");
        exit(1);
    }

    return varnum;
}

void
compile_program(struct compiler *compiler)
{
    struct function *startup_func = add_function(NULL, compiler, 0, 0);
    for (;;) {
        read_value(compiler->reader);
        if (compiler->reader->value.type == VAL_EOF)
            break;
        compile_form(startup_func, 1, &compiler->reader->value);
    }
    gen_code(startup_func, 1, "return VOID;\n");

    for (int i = 0; i < compiler->n_referenced_vars; ++i) {
        int found = 0;
        for (int j = 0; j < startup_func->n_params; ++j) {
            if (compiler->referenced_vars[i] == startup_func->params[j]) {
                found = 1;
                break;
            }
        }

        if (!found) {
            fprintf(stderr, "unbound identifier: %.*s\n",
                    compiler->reader->interned_name_len[compiler->referenced_vars[i]],
                    compiler->reader->interned_name[compiler->referenced_vars[i]]);
            exit(1);
        }
    }

    FILE *fp = fopen(compiler->c_filename, "w");
    compiler->output_file = fp;
    fprintf(fp, "#include <errno.h>\n");
    fprintf(fp, "#include <stdarg.h>\n");
    fprintf(fp, "#include <stddef.h>\n");
    fprintf(fp, "#include <stdint.h>\n");
    fprintf(fp, "#include <stdio.h>\n");
    fprintf(fp, "#include <stdlib.h>\n");
    fprintf(fp, "#include <string.h>\n");
    fprintf(fp, "\n");
    fprintf(fp, "typedef void *value;\n");
    fprintf(fp, "typedef value *environment;\n");
    fprintf(fp, "typedef struct closure *closure;\n");
    fprintf(fp, "typedef void(*kont)(value v);\n");
    fprintf(fp, "typedef value(*funcptr)(environment env, int nargs, ...);\n");
    fprintf(fp, "\n");
    fprintf(fp, "struct closure {\n");
    fprintf(fp, "    value (*func)(environment env, int nargs, ...);\n");
    fprintf(fp, "    int n_args;\n");
    fprintf(fp, "    int n_freevars;\n");
    fprintf(fp, "    value freevars[];\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "struct pair {\n");
    fprintf(fp, "    value car;\n");
    fprintf(fp, "    value cdr;\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "struct string {\n");
    fprintf(fp, "    size_t len;\n");
    fprintf(fp, "    char *s;\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "struct symbol {\n");
    fprintf(fp, "    size_t name_len;\n");
    fprintf(fp, "    char *name;\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "enum object_type {\n");
    fprintf(fp, "    OBJ_PORT,\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "enum port_direction {\n");
    fprintf(fp, "    PORT_DIR_READ,\n");
    fprintf(fp, "    PORT_DIR_WRITE,\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "struct object {\n");
    fprintf(fp, "    enum object_type type;\n");
    fprintf(fp, "    union {\n");
    fprintf(fp, "        struct {\n");
    fprintf(fp, "            int direction;\n");
    fprintf(fp, "            int closed;\n");
    fprintf(fp, "            FILE *fp;\n");
    fprintf(fp, "            value (*read_char)(value port);\n");
    fprintf(fp, "            value (*peek_char)(value port);\n");
    fprintf(fp, "            value (*read_line)(value port);\n");
    fprintf(fp, "            void (*write_char)(value port, value ch);\n");
    fprintf(fp, "            void (*printf)(value port, const char *fmt, ...);\n");
    fprintf(fp, "        } port;\n");
    fprintf(fp, "    };\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define FIXNUM_TAG 0x0\n");
    fprintf(fp, "#define OBJECT_TAG 0x01\n");
    fprintf(fp, "#define CLOSURE_TAG 0x02\n");
    fprintf(fp, "#define STRING_TAG 0x03\n");
    fprintf(fp, "#define PAIR_TAG 0x04\n");
    fprintf(fp, "#define VOID_TAG 0x15\n");   /*      10_101 */
    fprintf(fp, "#define BOOL_TAG 0xd\n");    /*       1_101 */
    fprintf(fp, "#define TRUE_TAG 0x1d\n");   /*      11_101 */
    fprintf(fp, "#define FALSE_TAG 0x0d\n");  /*      01_101 */
    fprintf(fp, "#define CHAR_TAG 0x25\n");   /*     100_101 */
    fprintf(fp, "#define SYMBOL_TAG 0x45\n"); /*    1000_101 */
    fprintf(fp, "#define NIL_TAG 0x85\n");    /*   10000_101 */
    fprintf(fp, "#define EOFOBJ_TAG 0x105\n"); /* 100000_101 */
    fprintf(fp, "\n");
    fprintf(fp, "#define TAG_MASK 0x7\n");
    fprintf(fp, "#define VALUE_MASK 0xfffffffffffffff8\n");
    fprintf(fp, "#define BOOL_TAG_MASK 0xf\n");
    fprintf(fp, "#define VOID_TAG_MASK 0x1f\n");
    fprintf(fp, "#define CHAR_TAG_MASK 0x3f\n");
    fprintf(fp, "#define SYMBOL_TAG_MASK 0x7f\n");
    fprintf(fp, "#define EOFOBJ_TAG_MASK 0x1ff\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define FIXNUM(v) (value)((uint64_t)(v) << 3 | FIXNUM_TAG)\n");
    fprintf(fp, "#define CLOSURE(v) (value)((uint64_t)(v) | CLOSURE_TAG)\n");
    fprintf(fp, "#define PAIR(v) (value)((uint64_t)(v) | PAIR_TAG)\n");
    fprintf(fp, "#define STRING(v) (value)((uint64_t)(v) | STRING_TAG)\n");
    fprintf(fp, "#define VOID (value)(VOID_TAG)\n");
    fprintf(fp, "#define TRUE (value)(TRUE_TAG)\n");
    fprintf(fp, "#define FALSE (value)(FALSE_TAG)\n");
    fprintf(fp, "#define BOOL(v) ((v) ? TRUE : FALSE)\n");
    fprintf(fp, "#define CHAR(v) (value)((uint64_t)(v) << 32 | CHAR_TAG)\n");
    fprintf(fp, "#define SYMBOL(v) (value)((uint64_t)(v) << 32 | SYMBOL_TAG)\n");
    fprintf(fp, "#define NIL (value)(NIL_TAG)\n");
    fprintf(fp, "#define EOFOBJ (value)(EOFOBJ_TAG)\n");
    fprintf(fp, "#define OBJECT(v) (value)((uint64_t)(v) | OBJECT_TAG)\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define GET_FIXNUM(v) ((int64_t)(v) >> 3)\n");
    fprintf(fp, "#define GET_CLOSURE(v) ((struct closure *)((uint64_t)(v) & VALUE_MASK))\n");
    fprintf(fp, "#define GET_BOOL(v) ((uint64_t)(v) >> 4)\n");
    fprintf(fp, "#define GET_STRING(v) ((struct string *)((uint64_t)(v) & VALUE_MASK))\n");
    fprintf(fp, "#define GET_PAIR(v) ((struct pair *)((uint64_t)(v) & VALUE_MASK))\n");
    fprintf(fp, "#define GET_CHAR(v) ((char)((uint64_t)(v) >> 32))\n");
    fprintf(fp, "#define GET_SYMBOL(v) ((int)((uint64_t)(v) >> 32))\n");
    fprintf(fp, "#define GET_OBJECT(v) ((struct object *)((uint64_t)(v) & VALUE_MASK))\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define IS_FIXNUM(v) (((uint64_t)(v) & TAG_MASK) == FIXNUM_TAG)\n");
    fprintf(fp, "#define IS_CLOSURE(v) (((uint64_t)(v) & TAG_MASK) == CLOSURE_TAG)\n");
    fprintf(fp, "#define IS_STRING(v) (((uint64_t)(v) & TAG_MASK) == STRING_TAG)\n");
    fprintf(fp, "#define IS_BOOL(v) (((uint64_t)(v) & BOOL_TAG_MASK) == BOOL_TAG)\n");
    fprintf(fp, "#define IS_VOID(v) (((uint64_t)(v) & VOID_TAG_MASK) == VOID_TAG)\n");
    fprintf(fp, "#define IS_CHAR(v) (((uint64_t)(v) & CHAR_TAG_MASK) == CHAR_TAG)\n");
    fprintf(fp, "#define IS_SYMBOL(v) (((uint64_t)(v) & SYMBOL_TAG_MASK) == SYMBOL_TAG)\n");
    fprintf(fp, "#define IS_NIL(v) ((uint64_t)(v) == NIL_TAG)\n");
    fprintf(fp, "#define IS_PAIR(v) (((uint64_t)(v) & TAG_MASK) == PAIR_TAG)\n");
    fprintf(fp, "#define IS_EOFOBJ(v) ((uint64_t)(v) == EOFOBJ_TAG)\n");
    fprintf(fp, "#define IS_OBJECT(v) (((uint64_t)(v) & TAG_MASK) == OBJECT_TAG)\n");
    fprintf(fp, "#define IS_PORT(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_PORT)\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define RAISE(...) { fprintf(stderr, \"exception: \" __VA_ARGS__); fprintf(stderr, \"\\n\"); cleanup(); exit(1); }\n");
    fprintf(fp, "\n");

    fprintf(fp, "static int n_symbols = 0;\n");
    fprintf(fp, "static struct symbol *symbols = NULL;\n");
    fprintf(fp, "\n");
    for (int i = 0; i < compiler->n_symbols; ++i) {
        fprintf(fp, "#define sym%.*s SYMBOL(%d)\n",
                compiler->reader->interned_mangled_len[compiler->symbols[i]],
                compiler->reader->interned_mangled[compiler->symbols[i]],
                i);
    }
    fprintf(fp, "\n");

    fprintf(fp, "static struct object current_input_port;\n");
    fprintf(fp, "static struct object current_output_port;\n");
    fprintf(fp, "static struct object current_error_port;\n");
    fprintf(fp, "\n");

    fprintf(fp, "static void cleanup(void) {}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value envget(environment env, int index) {\n");
    fprintf(fp, "    value *vars = env;\n");
    fprintf(fp, "    return vars[index];\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value make_closure(funcptr func, int nargs, int nfreevars, ...) {\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    struct closure *closure = calloc(1, sizeof(struct closure) + nfreevars * sizeof(value));\n");
    fprintf(fp, "    closure->func = func;\n");
    fprintf(fp, "    closure->n_args = nargs;\n");
    fprintf(fp, "    closure->n_freevars = nfreevars;\n");
    fprintf(fp, "    va_start(args, nfreevars);\n");
    fprintf(fp, "    for (int i = 0; i < nfreevars; ++i) {\n");
    fprintf(fp, "        closure->freevars[i] = va_arg(args, value);\n");
    fprintf(fp, "    };\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return CLOSURE(closure);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value make_pair(value car, value cdr) {\n");
    fprintf(fp, "    struct pair *pair = malloc(sizeof(struct pair));\n");
    fprintf(fp, "    pair->car = car;\n");
    fprintf(fp, "    pair->cdr = cdr;\n");
    fprintf(fp, "    return PAIR(pair);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "static value reverse_list(value list, value acc) {\n");
    fprintf(fp, "    if (list == NIL) {\n");
    fprintf(fp, "        return acc;\n");
    fprintf(fp, "    } else {\n");
    fprintf(fp, "        struct pair *p = GET_PAIR(list);\n");
    fprintf(fp, "        return reverse_list(p->cdr, make_pair(p->car, acc));\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value make_string(const char *s, size_t len) {\n");
    fprintf(fp, "    struct string *p = malloc(sizeof(struct string));\n");
    fprintf(fp, "    p->s = malloc(len + 1);\n");
    fprintf(fp, "    p->len = len;\n");
    fprintf(fp, "    memcpy(p->s, s, len);\n");
    fprintf(fp, "    p->s[len] = 0;\n");
    fprintf(fp, "    return STRING(p);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value file_read_line(value port) {\n");
    fprintf(fp, "    if (!IS_PORT(port)) { RAISE(\"reading from non-port\"); }\n");
    fprintf(fp, "    if (GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE(\"port not open for reading\") }");
    fprintf(fp, "    if (GET_OBJECT(port)->port.closed) { RAISE(\"the port is closed\") }\n");
    fprintf(fp, "    FILE *fp = GET_OBJECT(port)->port.fp;\n");
    fprintf(fp, "    char buf[256];\n");
    fprintf(fp, "    char *r = fgets(buf, sizeof(buf), fp);\n");
    fprintf(fp, "    if (!r) {\n");
    fprintf(fp, "        if (feof(fp)) { return EOFOBJ; };\n");
    fprintf(fp, "        RAISE(\"cannot read from file: %%s\", strerror(errno));\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "\n");
    fprintf(fp, "    size_t len = strlen(buf);\n");
    fprintf(fp, "    if (buf[len-1] == '\\n') len--;\n");
    fprintf(fp, "    struct string *str = GET_STRING(make_string(buf, len));\n");
    fprintf(fp, "\n");
    fprintf(fp, "    while (len == sizeof(buf) - 1) {\n");
    fprintf(fp, "        r = fgets(buf, sizeof(buf), fp);\n");
    fprintf(fp, "        if (!r && !feof(fp)) {\n");
    fprintf(fp, "            RAISE(\"cannot read from file: %%s\", strerror(errno));\n");
    fprintf(fp, "        }\n");
    fprintf(fp, "\n");
    fprintf(fp, "        len = strlen(buf);\n");
    fprintf(fp, "        if (buf[len-1] == '\\n') len--;\n");
    fprintf(fp, "        str->s = realloc(str->s, str->len + len + 1);\n");
    fprintf(fp, "        memcpy(str->s + str->len, buf, len + 1);\n");
    fprintf(fp, "        str->len += len;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "\n");
    fprintf(fp, "    return STRING(str);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value file_read_char(value port) {\n");
    fprintf(fp, "    if (!IS_PORT(port)) { RAISE(\"reading from non-port\"); }\n");
    fprintf(fp, "    if (GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE(\"port not open for reading\") }");
    fprintf(fp, "    if (GET_OBJECT(port)->port.closed) { RAISE(\"the port is closed\") }\n");
    fprintf(fp, "    FILE *fp = GET_OBJECT(port)->port.fp;\n");
    fprintf(fp, "    char ch = getc(fp);\n");
    fprintf(fp, "    if (ch == EOF) return EOFOBJ;\n");
    fprintf(fp, "    return CHAR(ch);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value file_peek_char(value port) {\n");
    fprintf(fp, "    if (!IS_PORT(port)) { RAISE(\"peeking non-port\"); }\n");
    fprintf(fp, "    if (GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE(\"port not open for reading\") }");
    fprintf(fp, "    if (GET_OBJECT(port)->port.closed) { RAISE(\"the port is closed\") }\n");
    fprintf(fp, "    FILE *fp = GET_OBJECT(port)->port.fp;\n");
    fprintf(fp, "    char ch = getc(fp);\n");
    fprintf(fp, "    if (ch == EOF) return EOFOBJ;\n");
    fprintf(fp, "    ungetc(ch, fp);\n");
    fprintf(fp, "    return CHAR(ch);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void file_write_char(value port, value ch) {\n");
    fprintf(fp, "    if (!IS_PORT(port)) { RAISE(\"writing to non-port\"); }\n");
    fprintf(fp, "    if (GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE(\"port not open for writing\") }");
    fprintf(fp, "    if (GET_OBJECT(port)->port.closed) { RAISE(\"the port is closed\") }\n");
    fprintf(fp, "    FILE *fp = GET_OBJECT(port)->port.fp;\n");
    fprintf(fp, "    int ret = putc(GET_CHAR(ch), fp);\n");
    fprintf(fp, "    if (ret == EOF) { RAISE(\"error writing to file: %%s\", strerror(errno)); }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void file_printf(value port, const char *fmt, ...) {\n");
    fprintf(fp, "    if (!IS_PORT(port)) { RAISE(\"writing to non-port\"); }\n");
    fprintf(fp, "    if (GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE(\"port not open for writing\") }");
    fprintf(fp, "    if (GET_OBJECT(port)->port.closed) { RAISE(\"the port is closed\") }\n");
    fprintf(fp, "    FILE *fp = GET_OBJECT(port)->port.fp;\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, fmt);\n");
    fprintf(fp, "    vfprintf(fp, fmt, args);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void _display(value v, value port);\n");
    fprintf(fp, "static void _display_pair(struct pair *v, value port, int in_the_middle) {\n");
    fprintf(fp, "    if (!in_the_middle) GET_OBJECT(port)->port.printf(port, \"(\");\n");
    fprintf(fp, "    _display(v->car, port);\n");
    fprintf(fp, "    if (IS_NIL(v->cdr)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \")\");\n");
    fprintf(fp, "    } else if (IS_PAIR(v->cdr)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \" \");\n");
    fprintf(fp, "        _display_pair(GET_PAIR(v->cdr), port, 1);\n");
    fprintf(fp, "    } else {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \" . \");\n");
    fprintf(fp, "        _display(v->cdr, port);\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \")\");\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void _display(value v, value port) {\n");
    fprintf(fp, "    if (IS_FIXNUM(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%ld\", GET_FIXNUM(v));\n");
    fprintf(fp, "    } else if (IS_STRING(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%.*s\", (int) GET_STRING(v)->len, GET_STRING(v)->s);\n");
    fprintf(fp, "    } else if (IS_SYMBOL(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%.*s\", (int) symbols[GET_SYMBOL(v)].name_len, symbols[GET_SYMBOL(v)].name);\n");
    fprintf(fp, "    } else if (IS_BOOL(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%s\", GET_BOOL(v) ? \"#t\" : \"#f\");\n");
    fprintf(fp, "    } else if (IS_VOID(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<void>\");\n");
    fprintf(fp, "    } else if (IS_CHAR(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%c\", GET_CHAR(v));\n");
    fprintf(fp, "    } else if (IS_NIL(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"()\");\n");
    fprintf(fp, "    } else if (IS_PAIR(v)) {\n");
    fprintf(fp, "        _display_pair(GET_PAIR(v), port, 0);\n");
    fprintf(fp, "    } else if (IS_CLOSURE(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<procedure-%%d>\", GET_CLOSURE(v)->n_args);\n");
    fprintf(fp, "    } else if (IS_EOFOBJ(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<eof-object>\");\n");
    fprintf(fp, "    } else {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<object-%%p>\", v);\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void _write(value v, value port);\n");
    fprintf(fp, "static void _write_pair(struct pair *v, value port, int in_the_middle) {\n");
    fprintf(fp, "    if (!in_the_middle) GET_OBJECT(port)->port.printf(port, \"(\");\n");
    fprintf(fp, "    _write(v->car, port);\n");
    fprintf(fp, "    if (IS_NIL(v->cdr)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \")\");\n");
    fprintf(fp, "    } else if (IS_PAIR(v->cdr)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \" \");\n");
    fprintf(fp, "        _write_pair(GET_PAIR(v->cdr), port, 1);\n");
    fprintf(fp, "    } else {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \" . \");\n");
    fprintf(fp, "        _write(v->cdr, port);\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \")\");\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void _write_char_literal(value v, value port) {\n");
    fprintf(fp, "    char ch = GET_CHAR(v);\n");
    fprintf(fp, "    char buf[16];\n");
    fprintf(fp, "    char *text;\n");
    fprintf(fp, "    switch (ch) {\n");
    fprintf(fp, "    case '\\a':\n");
    fprintf(fp, "        text = \"#\\\\alarm\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case '\\b':\n");
    fprintf(fp, "        text = \"#\\\\backspace\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case '\\x7f':\n");
    fprintf(fp, "        text = \"#\\\\delete\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case '\\x1b':\n");
    fprintf(fp, "        text = \"#\\\\escape\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case '\\n':\n");
    fprintf(fp, "        text = \"#\\\\newline\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case '\\0':\n");
    fprintf(fp, "        text = \"#\\\\null\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case '\\r':\n");
    fprintf(fp, "        text = \"#\\\\return\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case ' ':\n");
    fprintf(fp, "        text = \"#\\\\space\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    case '\\t':\n");
    fprintf(fp, "        text = \"#\\\\tab\";\n");
    fprintf(fp, "        break;\n");
    fprintf(fp, "    default:\n");
    fprintf(fp, "        if (ch >= 32 && ch < 127)");
    fprintf(fp, "            sprintf(buf, \"%%c\", ch);\n");
    fprintf(fp, "        else");
    fprintf(fp, "            sprintf(buf, \"\\\\x%%02x\", (int) ch);\n");
    fprintf(fp, "        text = buf;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "\n");
    fprintf(fp, "    GET_OBJECT(port)->port.printf(port, \"%%s\", text);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void _write_string_literal(value v, value port) {\n");
    fprintf(fp, "    struct object *op = GET_OBJECT(port);\n");
    fprintf(fp, "    struct string *s = GET_STRING(v);\n");
    fprintf(fp, "    op->port.printf(port, \"\\\"\");\n");
    fprintf(fp, "    for (int i = 0; i < s->len; ++i) {\n");
    fprintf(fp, "        switch (s->s[i]) {\n");
    fprintf(fp, "        case '\\a':\n");
    fprintf(fp, "            op->port.printf(port, \"\\\\a\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\b':\n");
    fprintf(fp, "            op->port.printf(port, \"\\\\b\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\r':\n");
    fprintf(fp, "            op->port.printf(port, \"\\\\r\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\n':\n");
    fprintf(fp, "            op->port.printf(port, \"\\\\n\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\t':\n");
    fprintf(fp, "            op->port.printf(port, \"\\\\t\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\"':\n");
    fprintf(fp, "            op->port.printf(port, \"\\\"\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\\\':\n");
    fprintf(fp, "            op->port.printf(port, \"\\\\\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        default:\n");
    fprintf(fp, "            if (s->s[i] >= 32 && s->s[i] < 127)\n");
    fprintf(fp, "                op->port.printf(port, \"%%c\", s->s[i]);\n");
    fprintf(fp, "            else\n");
    fprintf(fp, "                op->port.printf(port, \"\\\\x%%02x\", (int) s->s[i]);\n");
    fprintf(fp, "        }\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    op->port.printf(port, \"\\\"\");\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static void _write(value v, value port) {\n");
    fprintf(fp, "    if (IS_FIXNUM(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%ld\", GET_FIXNUM(v));\n");
    fprintf(fp, "    } else if (IS_STRING(v)) {\n");
    fprintf(fp, "        _write_string_literal(v, port);\n");
    fprintf(fp, "    } else if (IS_SYMBOL(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%.*s\", (int) symbols[GET_SYMBOL(v)].name_len, symbols[GET_SYMBOL(v)].name);\n");
    fprintf(fp, "    } else if (IS_BOOL(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"%%s\", GET_BOOL(v) ? \"#t\" : \"#f\");\n");
    fprintf(fp, "    } else if (IS_VOID(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<void>\");\n");
    fprintf(fp, "    } else if (IS_CHAR(v)) {\n");
    fprintf(fp, "        _write_char_literal(v, port);\n");
    fprintf(fp, "    } else if (IS_NIL(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"()\");\n");
    fprintf(fp, "    } else if (IS_PAIR(v)) {\n");
    fprintf(fp, "        _write_pair(GET_PAIR(v), port, 0);\n");
    fprintf(fp, "    } else if (IS_CLOSURE(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<procedure-%%d>\", GET_CLOSURE(v)->n_args);\n");
    fprintf(fp, "    } else if (IS_EOFOBJ(v)) {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<eof-object>\");\n");
    fprintf(fp, "    } else {\n");
    fprintf(fp, "        GET_OBJECT(port)->port.printf(port, \"#<object-%%p>\", v);\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value string_to_symbol(value v) {\n");
    fprintf(fp, "    for (int i = 0; i < n_symbols; ++i) {\n");
    fprintf(fp, "        if (symbols[i].name_len == GET_STRING(v)->len && memcmp(symbols[i].name, GET_STRING(v)->s, symbols[i].name_len) == 0) {\n");
    fprintf(fp, "            return SYMBOL(i);\n");
    fprintf(fp, "        }\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "\n");
    fprintf(fp, "    n_symbols++;\n");
    fprintf(fp, "    symbols = realloc(symbols, n_symbols * sizeof(struct symbol));\n");
    fprintf(fp, "    symbols[n_symbols - 1].name_len = GET_STRING(v)->len;\n");
    fprintf(fp, "    symbols[n_symbols - 1].name = malloc(GET_STRING(v)->len);\n");
    fprintf(fp, "    memcpy(symbols[n_symbols - 1].name, GET_STRING(v)->s, GET_STRING(v)->len);\n");
    fprintf(fp, "    return SYMBOL(n_symbols - 1);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value alloc_string(size_t len, char fill) {\n");
    fprintf(fp, "    struct string *str = malloc(sizeof(struct string));\n");
    fprintf(fp, "    str->len = len;\n");
    fprintf(fp, "    str->s = malloc(len);\n");
    fprintf(fp, "    memset(str->s, fill, len);\n");
    fprintf(fp, "    return STRING(str);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static int string_cmp(struct string *s1, struct string *s2) {\n");
    fprintf(fp, "    size_t min_len = s1->len < s2->len ? s1->len : s2->len;\n");
    fprintf(fp, "    int cmp = memcmp(s1->s, s2->s, min_len);\n");
    fprintf(fp, "    if (cmp != 0) return cmp;\n");
    fprintf(fp, "    if (s1->len < s2->len) {\n");
    fprintf(fp, "        return -1;\n");
    fprintf(fp, "    } else if (s1->len > s2->len) {\n");
    fprintf(fp, "        return 1;\n");
    fprintf(fp, "    } else {\n");
    fprintf(fp, "        return 0;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "/************ primcall functions ***********/\n");
    fprintf(fp, "static value primcall_car(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"car needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value arg = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PAIR(arg)) { RAISE(\"car argument is not a pair\") }\n");
    fprintf(fp, "    return GET_PAIR(arg)->car;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_cdr(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"car needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value arg = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PAIR(arg)) { RAISE(\"cdr argument is not a pair\") }\n");
    fprintf(fp, "    return GET_PAIR(arg)->cdr;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_close_port(environment env, int nargs, ...) {\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value port = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PORT(port)) { RAISE(\"close-port argument is not a port\") }\n");
    fprintf(fp, "    if (GET_OBJECT(port)->port.closed) return VOID;\n");
    fprintf(fp, "    int ret = fclose(GET_OBJECT(port)->port.fp);\n");
    fprintf(fp, "    if (ret) { RAISE(\"failed to close the port: %%s\", strerror(errno)); }\n");
    fprintf(fp, "    GET_OBJECT(port)->port.closed = 1;\n");
    fprintf(fp, "    return VOID;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_cons(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 2) { RAISE(\"cons needs two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value car = va_arg(args, value);\n");
    fprintf(fp, "    value cdr = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return make_pair(car, cdr);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_current_error_port(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 0) { RAISE(\"current-error-port needs no arguments\"); }\n");
    fprintf(fp, "    return OBJECT(&current_error_port);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_current_input_port(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 0) { RAISE(\"current-input-port needs no arguments\"); }\n");
    fprintf(fp, "    return OBJECT(&current_input_port);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_current_output_port(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 0) { RAISE(\"current-output-port needs no arguments\"); }\n");
    fprintf(fp, "    return OBJECT(&current_output_port);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_display(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1 && nargs != 2) { RAISE(\"display needs one or two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v = va_arg(args, value);\n");
    fprintf(fp, "    value port = nargs == 1 ? OBJECT(&current_output_port) : va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PORT(port)) { RAISE(\"writing to non-port\"); }\n");
    fprintf(fp, "    if (GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE(\"writing to non-output port\"); }\n");
    fprintf(fp, "    _display(v, port);\n");
    fprintf(fp, "    return VOID;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_eof_object_q(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"eof-object? needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return BOOL(IS_EOFOBJ(v));\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_eq_q(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 2) { RAISE(\"eq? needs two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v1 = va_arg(args, value);\n");
    fprintf(fp, "    value v2 = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return BOOL(v1 == v2);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_input_port_q(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"input-port? needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return BOOL(IS_PORT(v) && GET_OBJECT(v)->port.direction == PORT_DIR_READ);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_make_string(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1 && nargs != 2) { RAISE(\"make-string needs one or two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value n = va_arg(args, value);\n");
    fprintf(fp, "    value ch = nargs == 1 ? CHAR(0) : va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_FIXNUM(n)) { RAISE(\"make-string first argument should be a number\"); }\n");
    fprintf(fp, "    if (!IS_CHAR(ch)) { RAISE(\"make-string second argument should be a character\"); }\n");
    fprintf(fp, "    return alloc_string(GET_FIXNUM(n), GET_CHAR(ch));\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_number_to_string(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1 && nargs != 2) { RAISE(\"number->string needs one or two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value n = va_arg(args, value);\n");
    fprintf(fp, "    value base = nargs == 1 ? FIXNUM(10) : va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_FIXNUM(n)) { RAISE(\"number->string first argument should be a number\"); }\n");
    fprintf(fp, "    if (!IS_FIXNUM(base)) { RAISE(\"number->string second argument should be a number\"); }\n");
    fprintf(fp, "    char buf[128];\n");
    fprintf(fp, "    int start = 0;\n");
    fprintf(fp, "    int64_t m = GET_FIXNUM(n);\n");
    fprintf(fp, "    if (m < 0) { buf[0] = '-'; start = 1; m = -m; }\n");
    fprintf(fp, "    if (base == FIXNUM(10))\n");
    fprintf(fp, "        snprintf(buf + start, sizeof(buf), \"%%ld\", m);\n");
    fprintf(fp, "    else if (base == FIXNUM(16))\n");
    fprintf(fp, "        snprintf(buf + start, sizeof(buf), \"%%lx\", m);\n");
    fprintf(fp, "    else if (base == FIXNUM(8))\n");
    fprintf(fp, "        snprintf(buf + start, sizeof(buf), \"%%lo\", m);\n");
    fprintf(fp, "    else if (base == FIXNUM(2)) {\n");
    fprintf(fp, "        while (m >= 2) { buf[start++] = '0' + (m %% 2); m /= 2; }\n");
    fprintf(fp, "        buf[start++] = '0' + m;\n");
    fprintf(fp, "        buf[start] = 0;\n");
    fprintf(fp, "    } else\n");
    fprintf(fp, "        RAISE(\"radix not supported by number->string\");\n");
    fprintf(fp, "    return make_string(buf, strlen(buf));\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_open_input_file(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"open-input-file needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value filename = va_arg(args, value);");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_STRING(filename)) { RAISE(\"filename is not a string\"); }\n");
    fprintf(fp, "    int filename_len = GET_STRING(filename)->len;\n");
    fprintf(fp, "    char *filenamez = malloc(filename_len + 1);\n");
    fprintf(fp, "    snprintf(filenamez, filename_len + 1, \"%%.*s\", filename_len, GET_STRING(filename)->s);\n");
    fprintf(fp, "    FILE *fp = fopen(filenamez, \"r\");\n");
    fprintf(fp, "    if (!fp) { RAISE(\"error opening file: %%s\", strerror(errno)); }\n");
    fprintf(fp, "    struct object *obj = calloc(1, sizeof(struct object));\n");
    fprintf(fp, "    obj->type = OBJ_PORT;");
    fprintf(fp, "    obj->port.direction = PORT_DIR_READ;\n");
    fprintf(fp, "    obj->port.fp = fp;\n");
    fprintf(fp, "    obj->port.read_char = file_read_char;\n");
    fprintf(fp, "    obj->port.peek_char = file_peek_char;\n");
    fprintf(fp, "    obj->port.read_line = file_read_line;\n");
    fprintf(fp, "    return OBJECT(obj);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_open_output_file(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"open-output-file needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value filename = va_arg(args, value);");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_STRING(filename)) { RAISE(\"filename is not a string\"); }\n");
    fprintf(fp, "    int filename_len = GET_STRING(filename)->len;\n");
    fprintf(fp, "    char *filenamez = malloc(filename_len + 1);\n");
    fprintf(fp, "    snprintf(filenamez, filename_len + 1, \"%%.*s\", filename_len, GET_STRING(filename)->s);\n");
    fprintf(fp, "    FILE *fp = fopen(filenamez, \"w\");\n");
    fprintf(fp, "    if (!fp) { RAISE(\"error opening file: %%s\", strerror(errno)); }\n");
    fprintf(fp, "    struct object *obj = calloc(1, sizeof(struct object));\n");
    fprintf(fp, "    obj->type = OBJ_PORT;");
    fprintf(fp, "    obj->port.direction = PORT_DIR_WRITE;\n");
    fprintf(fp, "    obj->port.fp = fp;\n");
    fprintf(fp, "    obj->port.printf = file_printf;\n");
    fprintf(fp, "    return OBJECT(obj);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_peek_char(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 0 && nargs != 1) { RAISE(\"peek-char needs zero or one argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value port = nargs == 1 ? va_arg(args, value) : OBJECT(&current_input_port);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE(\"peek-char argument is not an input port\"); }\n");
    fprintf(fp, "    return GET_OBJECT(port)->port.peek_char(port);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_port_q(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"port? needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return BOOL(IS_PORT(v));\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_read_char(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 0 && nargs != 1) { RAISE(\"read-char needs zero or one argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value port = nargs == 1 ? va_arg(args, value) : OBJECT(&current_input_port);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE(\"read-char argument is not an input port\"); }\n");
    fprintf(fp, "    return GET_OBJECT(port)->port.read_char(port);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_read_line(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 0 && nargs != 1) { RAISE(\"read-line needs zero or one argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value port = nargs == 1 ? va_arg(args, value) : OBJECT(&current_input_port);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE(\"read-line argument is not an input port\"); }\n");
    fprintf(fp, "    return GET_OBJECT(port)->port.read_line(port);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_string_to_number(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1 && nargs != 2) { RAISE(\"string-to-number needs one or two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value str_v = va_arg(args, value);\n");
    fprintf(fp, "    value base = nargs == 1 ? FIXNUM(10) : va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_STRING(str_v)) { RAISE(\"string->number first argument must be a string\"); }\n");
    fprintf(fp, "    if (!IS_FIXNUM(base)) { RAISE(\"string->number second argument must be a number\"); }\n");
    fprintf(fp, "    if (GET_STRING(str_v)->len == 0) return FALSE;\n");
    fprintf(fp, "\n");
    fprintf(fp, "    /* create a zero terminated version of the string for strtoll */\n");
    fprintf(fp, "    char *str = malloc(GET_STRING(str_v)->len + 1);\n");
    fprintf(fp, "    memcpy(str, GET_STRING(str_v)->s, GET_STRING(str_v)->len);\n");
    fprintf(fp, "    str[GET_STRING(str_v)->len] = 0;\n");
    fprintf(fp, "\n");
    fprintf(fp, "    char *endptr;\n");
    fprintf(fp, "    int64_t result = strtoll(str, &endptr, GET_FIXNUM(base));\n");
    fprintf(fp, "    if (endptr != str + GET_STRING(str_v)->len) { free(str); return FALSE; }\n");
    fprintf(fp, "    free(str);\n");
    fprintf(fp, "    return FIXNUM(result);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_string_to_symbol(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"string->symbol needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value str = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_STRING(str)) { RAISE(\"string->symbol argument is not a string\"); }\n");
    fprintf(fp, "    return string_to_symbol(str);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_string_append(environment env, int nargs, ...) {\n");
    fprintf(fp, "    int total_size = 0;\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    for (int i = 0; i < nargs; ++i) {\n");
    fprintf(fp, "        struct string *str = GET_STRING(va_arg(args, value));");
    fprintf(fp, "        total_size += str->len;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "\n");
    fprintf(fp, "    struct string *concat = malloc(sizeof(struct string));\n");
    fprintf(fp, "    concat->len = total_size;\n");
    fprintf(fp, "    concat->s = malloc(total_size);\n");
    fprintf(fp, "\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    size_t offset = 0;\n");
    fprintf(fp, "    for (int i = 0; i < nargs; ++i) {\n");
    fprintf(fp, "        struct string *str = GET_STRING(va_arg(args, value));\n");
    fprintf(fp, "        memcpy(concat->s + offset, str->s, str->len);\n");
    fprintf(fp, "        offset += str->len;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "\n");
    fprintf(fp, "    return STRING(concat);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_string_length(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"string-length needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value str = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_STRING(str)) { RAISE(\"string-length argument is not a string\"); }\n");
    fprintf(fp, "    return FIXNUM(GET_STRING(str)->len);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_string_ref(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 2) { RAISE(\"string-ref needs two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value str = va_arg(args, value);\n");
    fprintf(fp, "    value idx = va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_STRING(str)) { RAISE(\"string-ref first argument is not a string\"); }\n");
    fprintf(fp, "    if (!IS_FIXNUM(idx)) { RAISE(\"string-ref second argument is not a number\"); }\n");
    fprintf(fp, "    if (GET_FIXNUM(idx) < 0 || GET_FIXNUM(idx) >= GET_STRING(str)->len) { RAISE(\"string-ref index is out of range\"); }\n");
    fprintf(fp, "    return CHAR(GET_STRING(str)->s[GET_FIXNUM(idx)]);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_string_eq_q(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs == 0) { RAISE(\"string=? needs at least one argument\"); }\n");
    fprintf(fp, "    if (nargs == 1) return TRUE;\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value prev = va_arg(args, value);");
    fprintf(fp, "    for (int i = 1; i < nargs; ++i) {\n");
    fprintf(fp, "        value cur = va_arg(args, value);");
    fprintf(fp, "        if (string_cmp(GET_STRING(prev), GET_STRING(cur)) != 0) {\n");
    fprintf(fp, "            va_end(args);\n");
    fprintf(fp, "            return FALSE;\n");
    fprintf(fp, "        }\n");
    fprintf(fp, "        prev = cur;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return TRUE;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_string_q(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"string? needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v = va_arg(args, value);");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return BOOL(IS_STRING(v));\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_symbol_q(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1) { RAISE(\"symbol? needs a single argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v = va_arg(args, value);");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    return BOOL(IS_SYMBOL(v));\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_write(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1 && nargs != 2) { RAISE(\"write needs one or two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value v = va_arg(args, value);\n");
    fprintf(fp, "    value port = nargs == 1 ? OBJECT(&current_output_port) : va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE(\"write second argument is not an output port\"); }\n");
    fprintf(fp, "    _write(v, port);\n");
    fprintf(fp, "    return VOID;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_write_char(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs != 1 && nargs != 2) { RAISE(\"write-char needs one or two arguments\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value ch = va_arg(args, value);\n");
    fprintf(fp, "    value port = nargs == 1 ? OBJECT(&current_output_port) : va_arg(args, value);\n");
    fprintf(fp, "    va_end(args);\n");
    fprintf(fp, "    if (!IS_CHAR(ch)) { RAISE(\"write-char first argument is not a char\"); }\n");
    fprintf(fp, "    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE(\"write-char second argument is not an output port\"); }\n");
    fprintf(fp, "    GET_OBJECT(port)->port.write_char(port, ch);\n");
    fprintf(fp, "    return VOID;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_add(environment env, int nargs, ...) {\n");
    fprintf(fp, "    value result = FIXNUM(0);\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    for (int i = 0; i < nargs; ++i) {\n");
    fprintf(fp, "        value v = va_arg(args, value);\n");
    fprintf(fp, "        if (!IS_FIXNUM(v)) { RAISE(\"addition (+) argument is not a number\") }\n");
    fprintf(fp, "        result += (int64_t) v;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    return result;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_div(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs < 1) { RAISE(\"division (-) needs at least one argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value result_v = va_arg(args, value);\n");
    fprintf(fp, "    if (!IS_FIXNUM(result_v)) { RAISE(\"division (/) argument is not a number\"); }\n");
    fprintf(fp, "    int64_t result = GET_FIXNUM(result_v);\n");
    fprintf(fp, "    if (nargs == 1) {");
    fprintf(fp, "        if (result == 1)\n");
    fprintf(fp, "            return FIXNUM(1);\n");
    fprintf(fp, "        if (result == -1)\n");
    fprintf(fp, "            return FIXNUM(-1);\n");
    fprintf(fp, "        if (result == 0)\n");
    fprintf(fp, "            RAISE(\"division by zero\");\n");
    fprintf(fp, "        return FIXNUM(0); /* we don't have fractionals, so 1/n is always zero */\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    for (int i = 1; i < nargs; ++i) {\n");
    fprintf(fp, "        value v = va_arg(args, value);\n");
    fprintf(fp, "        if (!IS_FIXNUM(v)) { RAISE(\"division (/) argument is not a number\") }\n");
    fprintf(fp, "        if (GET_FIXNUM(v) == 0)\n");
    fprintf(fp, "            RAISE(\"division by zero\");\n");
    fprintf(fp, "        result /= GET_FIXNUM(v);\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    return FIXNUM(result);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_mul(environment env, int nargs, ...) {\n");
    fprintf(fp, "    int64_t result = 1;\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    for (int i = 0; i < nargs; ++i) {\n");
    fprintf(fp, "        value v = va_arg(args, value);\n");
    fprintf(fp, "        if (!IS_FIXNUM(v)) { RAISE(\"multiplication (*) argument is not a number\") }\n");
    fprintf(fp, "        result *= GET_FIXNUM(v);\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    return FIXNUM(result);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value primcall_sub(environment env, int nargs, ...) {\n");
    fprintf(fp, "    if (nargs < 1) { RAISE(\"subtraction (-) needs at least one argument\"); }\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    va_start(args, nargs);\n");
    fprintf(fp, "    value result = va_arg(args, value);\n");
    fprintf(fp, "    if (!IS_FIXNUM(result)) { RAISE(\"subtraction (-) argument is not a number\"); }\n");
    fprintf(fp, "    if (nargs == 1) return FIXNUM(-GET_FIXNUM(result));");
    fprintf(fp, "    for (int i = 1; i < nargs; ++i) {\n");
    fprintf(fp, "        value v = va_arg(args, value);\n");
    fprintf(fp, "        if (!IS_FIXNUM(v)) { RAISE(\"subtraction (-) argument is not a number\") }\n");
    fprintf(fp, "        result -= (int64_t) v;\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "    return result;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");

    /* add function prototypes */
    for (int i = 0; i < compiler->n_functions; ++i) {
        fprintf(fp, "static value %s(environment env, int nargs, ...);\n", compiler->functions[i]->name);
    }
    fprintf(fp, "\n");

    /* add startup func variables as globals */
    for (int i = 0; i < startup_func->n_params; ++i) {
        fprintf(fp, "static value %.*s;\n",
                compiler->reader->interned_mangled_len[startup_func->params[i]],
                compiler->reader->interned_mangled[startup_func->params[i]]);
    }
    fprintf(fp, "\n");

    /* add function implementations */
    for (int i = 0; i < compiler->n_functions; ++i) {
        fprintf(fp, "static value %s(environment env, int nargs, ...) {\n", compiler->functions[i]->name);
        fwrite(compiler->functions[i]->code, 1, compiler->functions[i]->code_size, fp);
        fprintf(fp, "}\n\n");
    }

    fprintf(fp, "\n");
    fprintf(fp, "int main(int argc, const char *argv[]) {\n");

    fprintf(fp, "    n_symbols = %d;\n", compiler->n_symbols);
    fprintf(fp, "    symbols = malloc(sizeof(struct symbol) * %d);\n", compiler->n_symbols);
    for (int i = 0; i < compiler->n_symbols; ++i) {
        fprintf(fp, "    symbols[%d].name = \"%.*s\";\n", i,
                compiler->reader->interned_name_len[compiler->symbols[i]],
                compiler->reader->interned_name[compiler->symbols[i]]);
        fprintf(fp, "    symbols[%d].name_len = %d;\n", i, compiler->reader->interned_name_len[compiler->symbols[i]]);
    }
    fprintf(fp, "\n");

    fprintf(fp, "    current_input_port.type = OBJ_PORT;\n");
    fprintf(fp, "    current_input_port.port.direction = PORT_DIR_READ;\n");
    fprintf(fp, "    current_input_port.port.fp = stdin;\n");
    fprintf(fp, "    current_input_port.port.read_char = file_read_char;\n");
    fprintf(fp, "    current_input_port.port.peek_char = file_peek_char;\n");
    fprintf(fp, "    current_input_port.port.read_line = file_read_line;\n");
    fprintf(fp, "\n");

    fprintf(fp, "    current_output_port.type = OBJ_PORT;\n");
    fprintf(fp, "    current_output_port.port.direction = PORT_DIR_WRITE;\n");
    fprintf(fp, "    current_output_port.port.fp = stdout;\n");
    fprintf(fp, "    current_output_port.port.printf = file_printf;\n");
    fprintf(fp, "    current_output_port.port.write_char = file_write_char;\n");
    fprintf(fp, "\n");

    fprintf(fp, "    current_error_port.type = OBJ_PORT;\n");
    fprintf(fp, "    current_error_port.port.direction = PORT_DIR_WRITE;\n");
    fprintf(fp, "    current_error_port.port.fp = stderr;\n");
    fprintf(fp, "    current_error_port.port.printf = file_printf;\n");
    fprintf(fp, "    current_error_port.port.write_char = file_write_char;\n");
    fprintf(fp, "\n");

    fprintf(fp, "    %s(NULL, 0);\n", startup_func->name);
    fprintf(fp, "}\n");
    fclose(fp);
}

/*********************** printer ************************/

void
print_value(struct value *value)
{
    switch (value->type) {
    case VAL_NUM:
        fprintf(stderr, "<number %ld>", value->number);
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

struct arguments {
    const char *input_filename;
    const char *output_filename;
    int output_c;
    int run;

    const char *c_filename;
    const char *executable_filename;
    int delete_executable;
};

static error_t
parse_opt(int key, char *arg, struct argp_state *state)
{
    struct arguments *arguments = state->input;

    switch (key) {
    case 'c':
        arguments->output_c = 1;
        break;

    case 'o':
        arguments->output_filename = arg;
        break;

    case 'r':
        arguments->run = 1;
        break;

    case ARGP_KEY_ARG:
        if (state->arg_num == 0) {
            arguments->input_filename = arg;
        } else {
            /* too many positional arguments */
            argp_usage(state);
        }
        break;

    case ARGP_KEY_END:
        if (state->arg_num == 0) {
            /* filename not passed */
            argp_usage(state);
        }
        break;

    default:
        return ARGP_ERR_UNKNOWN;
    }

    return 0;
}

int
main(int argc, char const *argv[])
{
    const char *program;
    long program_length;

    struct arguments arguments;
    arguments.input_filename = NULL;
    arguments.output_filename = NULL;
    arguments.output_c = 0;
    arguments.run = 0;

    arguments.delete_executable = 0;
    arguments.c_filename = NULL;
    arguments.executable_filename = NULL;

    struct argp_option options[] = {
        { "run", 'r', 0, 0, "Run the output executable", },
        { "c", 'c', 0, 0, "Output a C file, not an executable"},
        { "output", 'o', "FILENAME", 0, "Output filename (C or executable depending on -c). Defaults to b.c or b.out" },
        { 0 },
    };
    struct argp argp = { options, parse_opt, "FILENAME", 0 };
    argp_parse(&argp, argc, (char**) argv, 0, 0, &arguments);

    if (arguments.output_c && arguments.run) {
        fprintf(stderr, "Cannot use both -r and -c\n");
        exit(1);
    }

    if (!arguments.output_filename) {
        arguments.delete_executable = 1;
        if (arguments.output_c) {
            arguments.output_filename = "b.c";
        } else {
            if (arguments.run) {
                /* we don't want to output an executable. create a temporary name. */
                int len = strlen("/tmp/whisper.XXXXXX");
                char *filename = malloc(len + 1);
                strcpy(filename, "/tmp/whisper.XXXXXX");
                int fd = mkstemp(filename);
                close(fd);

                arguments.output_filename = filename;
                arguments.delete_executable = 1;
            } else {
                arguments.output_filename = "b.out";
            }
        }
    }

    if (arguments.output_c) {
        arguments.c_filename = arguments.output_filename;
    } else {
        int len = strlen("/tmp/whisper.XXXXXX.c");
        char *c_filename = malloc(len + 1);
        strcpy(c_filename, "/tmp/whisper.XXXXXX");
        int fd = mkstemp(c_filename);
        close(fd);
        c_filename[strlen("/tmp/whisper.XXXXXX")] = '.';
        c_filename[strlen("/tmp/whisper.XXXXXX")+1] = 'c';
        c_filename[strlen("/tmp/whisper.XXXXXX")+2] = 0;
        arguments.c_filename = c_filename;

        arguments.executable_filename = arguments.output_filename;
    }

    program = read_file(arguments.input_filename, &program_length);
    struct lexer lexer = {
        .filename = arguments.input_filename,
        .program = program,
        .program_length = program_length,
        .ptr = program,
    };

    struct reader *reader = create_reader(&lexer);

    read_token(&lexer);
    struct compiler compiler = {
        .reader = reader,
        .c_filename = arguments.c_filename,
    };
    compile_program(&compiler);

    if (!arguments.output_c) {
        char command[256];
        char *cc;

        cc = getenv("CC");
        if (!cc) {
            cc = "gcc";
        }

        snprintf(command, sizeof(command), "%s -o %s %s",
                 cc, arguments.executable_filename, compiler.c_filename);
        int ret = system(command);
        if (ret) {
            fprintf(stderr, "Error compiling output C file.\n");
            exit(1);
        }

        unlink(arguments.c_filename);
    }

    if (arguments.run) {
        char command[256];

        /* we do this because if output file is something like b.out, then we can't just ask
         * shell to execute it, since it's not in the path. */
        snprintf(command, sizeof(command), "$(realpath %s)", arguments.executable_filename);
        int ret = system(command);

        if (arguments.delete_executable) {
            unlink(arguments.executable_filename);
        }

        return ret;
    }

    return 0;
}
