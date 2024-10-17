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

            *dst = '_';
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
            }
        } else {
            reader->value.type = VAL_ID;
        }

        read_token(reader->lexer);

        break;
    case TOK_EOF:
        fprintf(stderr, "internal error: read_value called on EOF\n");
        exit(1);
    default:
        fprintf(stderr, "read error\n");
        exit(1);
    }
}

/*********************** compiler ************************/

struct function
{
    struct compiler *compiler;
    char *name;

    char *code;
    int code_size;

    /* the counter used for naming variable names inside a function */
    int varnum;

    int n_freevars;
    interned_string *freevars;

    int n_params;
    interned_string params[];
};

struct compiler
{
    struct reader *reader;
    const char *output_filename;
    FILE *output_file;

    int n_functions;
    struct function **functions;

    int n_symbols;
    interned_string *symbols;
};

int compile_form(struct function *func, struct value *form);

void
add_compiled_symbol(struct compiler *compiler, interned_string identifier)
{
    compiler->n_symbols++;
    compiler->symbols = realloc(compiler->symbols, sizeof(interned_string) * compiler->n_symbols);
    compiler->symbols[compiler->n_symbols - 1] = identifier;
}

void
gen_code(struct function *func, const char *fmt, ...)
{
    va_list args;
    char buf[2048];
    int len;

    va_start(args, fmt);
    vsnprintf(buf, sizeof(buf), fmt, args);
    va_end(args);

    len = strlen(buf);
    func->code = realloc(func->code, func->code_size + len);
    memcpy(func->code + func->code_size, buf, len);
    func->code_size += len;
}

struct function *
add_function(struct compiler *compiler, int nparams)
{
    char name_buf[1024];
    int name_len;

    struct function *func = calloc(1, sizeof(struct function) + nparams * sizeof(interned_string));
    func->compiler = compiler;
    func->code_size = 0;
    func->code = NULL;
    func->n_params = nparams;

    snprintf(name_buf, sizeof(name_buf), "f%d", compiler->n_functions++);
    name_len = strlen(name_buf);
    func->name = malloc(name_len + 1);
    memcpy(func->name, name_buf, name_len + 1);

    compiler->functions = realloc(compiler->functions, compiler->n_functions);
    compiler->functions[compiler->n_functions - 1] = func;

    return func;
}

int
compile_number(struct function *func, struct value *form)
{
    int varnum = func->varnum++;
    gen_code(func, "    value x%d = FIXNUM(%lld);\n", varnum, form->number);
    return varnum;
}

int
compile_bool(struct function *func, struct value *form)
{
    int varnum = func->varnum++;
    if (form->bool) {
        gen_code(func, "    value x%d = TRUE;\n", varnum);
    } else {
        gen_code(func, "    value x%d = FALSE;\n", varnum);
    }

    return varnum;
}

int
compile_identifier(struct function *func, struct value *form)
{
    int found = 0;
    int varnum = func->varnum++;

    for (int i = 0; i < func->n_params; ++i) {
        if (func->params[i] == form->identifier.interned) {
            gen_code(func, "    value x%d = %.*s;\n",
                     varnum,
                     func->compiler->reader->interned_mangled_len[form->identifier.interned],
                     func->compiler->reader->interned_mangled[form->identifier.interned]);
            found = 1;
            break;
        }
    }

    if (!found) {
        /* it's a free variable */
        for (int j = 0; j < func->n_freevars; ++j) {
            if (func->freevars[j] == form->identifier.interned) {
                /* it's been used before */
                gen_code(func, "    value x%d = envget(env, %d);\n", varnum, j);
                found = 1;
                break;
            }
        }

        if (!found) {
            func->n_freevars++;
            func->freevars = realloc(func->freevars, func->n_freevars * sizeof(interned_string));
            func->freevars[func->n_freevars - 1] = form->identifier.interned;
            gen_code(func, "    value x%d = envget(env, %d);\n", varnum, func->n_freevars - 1);
        }
    }

    return varnum;
}

int
compile_string(struct function *func, struct value *form)
{
    int varnum = func->varnum++;

    gen_code(func, "    value x%d = make_string(\"", varnum);

    for (int i = 0; i < form->string.length; ++i) {
        char c = form->string.ptr[i];
        switch (c) {
        case '\b':
            gen_code(func, "\\b");
            break;
        case '\n':
            gen_code(func, "\\n");
            break;
        case '\r':
            gen_code(func, "\\r");
            break;
        case '\t':
            gen_code(func, "\\t");
            break;
        case '\\':
            gen_code(func, "\\\\");
            break;
        case '\"':
            gen_code(func, "\\\"");
            break;
        case '\0':
            gen_code(func, "\\0");
            break;
        default:
            if (c >= 32 && c < 127) {
                /* printable */
                gen_code(func, "%c", c);
            } else {
                /* use octal form for everything else */
                gen_code(func, "\\%03o", (unsigned char) c);
            }
        }
    }

    gen_code(func, "\", %d);\n", form->string.length);

    return varnum;
}

int
compile_char(struct function *func, struct value *form)
{
    int varnum = func->varnum++;
    char c = form->character;

    gen_code(func, "    value x%d = CHAR('", varnum);

    switch (c) {
    case '\b':
        gen_code(func, "\\b");
        break;
    case '\n':
        gen_code(func, "\\n");
        break;
    case '\r':
        gen_code(func, "\\r");
        break;
    case '\t':
        gen_code(func, "\\t");
        break;
    case '\\':
        gen_code(func, "\\\\");
        break;
    case '\'':
        gen_code(func, "\\\'");
        break;
    case '\0':
        gen_code(func, "\0");
        break;
    default:
        if (c >= 32 && c < 127) {
            gen_code(func, "%c", c);
        } else {
            gen_code(func, "\\%03o", (unsigned char) c);
        }
    }

    gen_code(func, "');\n");
    return varnum;
}

int
compile_car(struct function *func, struct value *form)
{
    if (form->list.length != 2) {
        fprintf(stderr, "car expects a single argument\n");
        exit(1);
    }

    int arg_varnum = compile_form(func, &form->list.ptr[0]);
    int dst_varnum = func->varnum++;
    gen_code(func, "    value x%d = car(x%d);", dst_varnum, arg_varnum);
    return dst_varnum;
}

int
compile_cdr(struct function *func, struct value *form)
{
    if (form->list.length != 2) {
        fprintf(stderr, "cdr expects a single argument\n");
        exit(1);
    }

    int arg_varnum = compile_form(func, &form->list.ptr[0]);
    int dst_varnum = func->varnum++;
    gen_code(func, "    value x%d = cdr(x%d);", dst_varnum, arg_varnum);
    return dst_varnum;
}

int
compile_cons(struct function *func, struct value *form)
{
    if (form->list.length != 3) {
        fprintf(stderr, "cons expects a two arguments\n");
        exit(1);
    }

    int car_varnum = compile_form(func, &form->list.ptr[1]);
    int cdr_varnum = compile_form(func, &form->list.ptr[2]);
    int dst_varnum = func->varnum++;
    gen_code(func, "    value x%d = make_pair(x%d, x%d);\n", dst_varnum, car_varnum, cdr_varnum);
    return dst_varnum;
}

int
compile_add(struct function *func, struct value *form)
{
    int dst_varnum;
    int arg_varnum;

    if (form->list.length == 1) {
        dst_varnum = func->varnum++;
        gen_code(func, "    value x%d = 0;\n", dst_varnum);
    }

    arg_varnum = compile_form(func, &form->list.ptr[1]);
    dst_varnum = arg_varnum;
    for (int i = 2; i < form->list.length; ++i) {
        dst_varnum = compile_form(func, &form->list.ptr[i]);
        gen_code(func, "    x%d += (int64_t) x%d;\n", dst_varnum, arg_varnum);
        arg_varnum = dst_varnum;
    }

    return dst_varnum;
}

int
compile_function(struct function *func, struct value *form,
                 int params_idx, int body_start_idx)
{
    int varnum = func->varnum++;

    struct value *params = &form->list.ptr[params_idx];

    if (params->type != VAL_LIST || params->list.tail != NULL) {
        fprintf(stderr, "rest parameters not supported yet\n");
        exit(1);
    }

    struct function *new_func = add_function(func->compiler, form->list.ptr[1].list.length);
    for (int i = 0; i < params->list.length; ++i) {
        new_func->params[i] = params->list.ptr[i].identifier.interned;
    }

    gen_code(new_func, "    va_list args;\n");
    gen_code(new_func, "    va_start(args, nargs);\n");
    for (int i = 0; i < params->list.length; ++i) {
        gen_code(new_func, "    value %.*s = va_arg(args, value);\n",
                func->compiler->reader->interned_mangled_len[params->list.ptr[i].identifier.interned],
                func->compiler->reader->interned_mangled[params->list.ptr[i].identifier.interned]);
    }
    gen_code(new_func, "    va_end(args);\n");
    gen_code(new_func, "\n");

    for (int i = body_start_idx; i < form->list.length; ++i) {
        compile_form(new_func, &form->list.ptr[i]);
    }

    /* now generate to code for referencing the function */
    gen_code(func, "    value x%d = make_closure(%s, %d, %d",
             varnum,
             new_func->name,
             params->list.length,
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

        gen_code(func, ", %.*s", fvar_len, fvar);
    }
    gen_code(func, ");\n");

    return varnum;
}

int
compile_lambda(struct function *func, struct value *form)
{
    if (form->list.length < 3) {
        fprintf(stderr, "invalid lambda form\n");
        exit(1);
    }

    return compile_function(func, form, 1, 2);
}

int
compile_call(struct function *func, struct value *form)
{
    int ret_varnum;
    int func_varnum;
    int *arg_varnums;

    func_varnum = compile_form(func, &form->list.ptr[0]);

    arg_varnums = malloc(sizeof(int) * (form->list.length - 1));
    for (int i = 1; i < form->list.length; ++i) {
        arg_varnums[i - 1] = compile_form(func, &form->list.ptr[i]);
    }

    ret_varnum = func->varnum++;
    gen_code(func, "    value x%d = GET_CLOSURE(x%d)->func(GET_CLOSURE(x%d)->freevars, %d", ret_varnum, func_varnum, func_varnum, form->list.length - 1);
    for (int i = 0; i < form->list.length - 1; ++i) {
        gen_code(func, ", x%d", arg_varnums[i]);
    }
    gen_code(func, ");\n");

    return ret_varnum;
}

int
compile_display(struct function *func, struct value *form)
{
    if (form->list.length != 2) {
        fprintf(stderr, "display expects a single argument\n");
        exit(1);
    }

    int arg_varnum = compile_form(func, &form->list.ptr[1]);

    int ret_varnum = func->varnum++;
    gen_code(func, "    value x%d = display(x%d);\n", ret_varnum, arg_varnum);

    return ret_varnum;
}

int
compile_define(struct function *func, struct value *form)
{
    int varnum;

    if (form->list.length < 2) {
        fprintf(stderr, "malformed define\n");
        exit(1);
    }

    if (form->list.ptr[0].type != VAL_ID) {
        fprintf(stderr, "malformed define\n");
        exit(1);
    }

    int var_name = form->list.ptr[1].identifier.interned;
    int mangled_len = func->compiler->reader->interned_mangled_len[var_name];
    char *mangled_str = func->compiler->reader->interned_mangled[var_name];
    func->n_params++;
    func->params[func->n_params - 1] = var_name;

    if (form->list.length == 2) {
        /* define the variable with a void initial value */
        gen_code(func, "    value %.*s = VOID;\n", mangled_len, mangled_str);
        varnum = func->varnum++;
        gen_code(func, "    value x%d = %.*s;\n", mangled_len, mangled_str);

        return varnum;
    }

    if (form->list.length == 3) {
        varnum = compile_form(func, &form->list.ptr[2]);
        gen_code(func, "    value %.*s = x%d;\n", mangled_len, mangled_str, varnum);

        return varnum;
    }

    /* compile lambda does not check for the "lambda" symbol at the head
     * of the list, so it can compile a "define" form as lambda. */
    varnum = compile_function(func, form, 2, 3);
    gen_code(func, "    value %.*s = x%d;\n", mangled_len, mangled_str, varnum);

    return varnum;
}

int
compile_if(struct function *func, struct value *form)
{
    if (form->list.length < 3 || form->list.length > 4) {
        fprintf(stderr, "malformed if\n");
        exit(1);
    }

    int cond_varnum = compile_form(func, &form->list.ptr[1]);
    int then_varnum = compile_form(func, &form->list.ptr[2]);
    int else_varnum;
    int ret_varnum;
    if (form->list.length == 3) {
        ret_varnum = func->varnum++;
        gen_code(func, "    value x%d = VOID;\n", ret_varnum);
        gen_code(func, "    if (GET_BOOL(x%d)) {\n", cond_varnum);
        gen_code(func, "        x%d = x%d;\n", ret_varnum, then_varnum);
        gen_code(func, "    }\n");
    } else {
        ret_varnum = func->varnum++;
        gen_code(func, "    value x%d;\n", ret_varnum);
        gen_code(func, "    if (GET_BOOL(x%d)) {\n", cond_varnum);
        gen_code(func, "        x%d = x%d;\n", ret_varnum, then_varnum);
        gen_code(func, "    } else {\n");
        else_varnum = compile_form(func, &form->list.ptr[3]);
        gen_code(func, "        x%d = x%d;\n", ret_varnum, else_varnum);
        gen_code(func, "    }\n");
    }

    return ret_varnum;
}

int
compile_begin(struct function *func, struct value *form)
{
    int ret_varnum = func->varnum++;
    gen_code(func, "    value x%d = VOID;\n", ret_varnum);

    for (int i = 1; i < form->list.length; ++i) {
        int arg_varnum = compile_form(func, &form->list.ptr[i]);
        if (i == form->list.length - 1) {
            gen_code(func, "    x%d = x%d;\n", ret_varnum, arg_varnum);
        }
    }

    return ret_varnum;
}

int
compile_quote(struct function *func, struct value *form)
{
    int varnum;
    struct value *value = &form->list.ptr[1];

    switch (value->type) {
    case VAL_BOOL:
    case VAL_NUM:
    case VAL_CHAR:
    case VAL_STR:
        varnum = compile_form(func, value);

    case VAL_ID:
        varnum = func->varnum++;
        add_compiled_symbol(func->compiler, value->identifier.interned);
        gen_code(func, "    value x%d = tok%.*s;\n", varnum,
                 func->compiler->reader->interned_mangled_len[value->identifier.interned],
                 func->compiler->reader->interned_mangled[value->identifier.interned]);
        break;
    default:
        fprintf(stderr, "unknown quoted value type\n");
        exit(1);
    }

    return varnum;
}

int
compile_list(struct function *func, struct value *form)
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
        varnum = compile_car(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 3 &&
               memcmp(list_car->identifier.name, "cdr", 3) == 0)
    {
        varnum = compile_cdr(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 4 &&
               memcmp(list_car->identifier.name, "cons", 4) == 0)
    {
        varnum = compile_cons(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 5 &&
               memcmp(list_car->identifier.name, "quote", 5) == 0)
    {
        varnum = compile_quote(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 7 &&
               memcmp(list_car->identifier.name, "display", 7) == 0)
    {
        varnum = compile_display(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 1 &&
               list_car->identifier.name[0] == '+')
    {
        varnum = compile_add(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 6 &&
               memcmp(list_car->identifier.name, "lambda", 5) == 0)
    {
        varnum = compile_lambda(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 6 &&
               memcmp(list_car->identifier.name, "define", 5) == 0)
    {
        varnum = compile_define(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 2 &&
               memcmp(list_car->identifier.name, "if", 2) == 0)
    {
        varnum = compile_if(func, form);
    } else if (list_car->type == VAL_ID &&
               list_car->identifier.name_len == 5 &&
               memcmp(list_car->identifier.name, "begin", 5) == 0)
    {
        varnum = compile_begin(func, form);
    } else {
        varnum = compile_call(func, form);
    }

    return varnum;
 }

int
compile_form(struct function *func, struct value *form)
{
    int varnum;
    if (form->type == VAL_NUM) {
        varnum = compile_number(func, form);
    } else if (form->type == VAL_ID) {
        varnum = compile_identifier(func, form);
    } else if (form->type == VAL_STR) {
        varnum = compile_string(func, form);
    } else if (form->type == VAL_BOOL) {
        varnum = compile_bool(func, form);
    } else if (form->type == VAL_LIST) {
        varnum = compile_list(func, form);
    } else if (form->type == VAL_CHAR) {
        varnum = compile_char(func, form);
    } else {
        fprintf(stderr, "unhandled value type\n");
        exit(1);
    }

    return varnum;
}

void
compile_program(struct compiler *compiler)
{
    struct function *startup_func = add_function(compiler, 0);
    while (compiler->reader->lexer->cur_tok_type != TOK_EOF) {
        read_value(compiler->reader);
        compile_form(startup_func, &compiler->reader->value);

        if (startup_func->n_freevars > 0) {
            for (int i = 0; i < startup_func->n_freevars; ++i) {
                fprintf(stderr, "unbound identifier: %.*s\n",
                        compiler->reader->interned_name_len[startup_func->freevars[i]],
                        compiler->reader->interned_name[startup_func->freevars[i]]);
            }

            exit(1);
        }
    }

    FILE *fp = fopen(compiler->output_filename, "w");
    compiler->output_file = fp;
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
    fprintf(fp, "    char s[];\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "struct symbol {\n");
    fprintf(fp, "    size_t name_len;\n");
    fprintf(fp, "    char *name;\n");
    fprintf(fp, "};\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define FIXNUM_TAG 0x0\n");
    fprintf(fp, "#define CLOSURE_TAG 0x02\n");
    fprintf(fp, "#define STRING_TAG 0x03\n");
    fprintf(fp, "#define PAIR_TAG 0x04\n");
    fprintf(fp, "#define VOID_TAG 0x15\n");   /*   10_101 */
    fprintf(fp, "#define BOOL_TAG 0xd\n");    /*    1_101 */
    fprintf(fp, "#define TRUE_TAG 0x1d\n");   /*   11_101 */
    fprintf(fp, "#define FALSE_TAG 0x0d\n");  /*   01_101 */
    fprintf(fp, "#define CHAR_TAG 0x25\n");   /*  100_101 */
    fprintf(fp, "#define SYMBOL_TAG 0x25\n"); /* 1000_101 */
    fprintf(fp, "\n");
    fprintf(fp, "#define TAG_MASK 0x3\n");
    fprintf(fp, "#define VALUE_MASK 0xfffffffffffffff8\n");
    fprintf(fp, "#define BOOL_TAG_MASK 0xf\n");
    fprintf(fp, "#define VOID_TAG_MASK 0x1f\n");
    fprintf(fp, "#define CHAR_TAG_MASK 0x3f\n");
    fprintf(fp, "#define SYMBOL_TAG_MASK 0x7f\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define FIXNUM(v) (value)((uint64_t)(v) << 3 | FIXNUM_TAG)\n");
    fprintf(fp, "#define CLOSURE(v) (value)((uint64_t)(v) | CLOSURE_TAG)\n");
    fprintf(fp, "#define PAIR(v) (value)((uint64_t)(v) | PAIR_TAG)\n");
    fprintf(fp, "#define STRING(v) (value)((uint64_t)(v) | STRING_TAG)\n");
    fprintf(fp, "#define VOID (value)(VOID_TAG)\n");
    fprintf(fp, "#define TRUE (value)(TRUE_TAG)\n");
    fprintf(fp, "#define FALSE (value)(FALSE_TAG)\n");
    fprintf(fp, "#define CHAR(v) (value)((uint64_t)(v) << 32 | CHAR_TAG)\n");
    fprintf(fp, "#define SYMBOL(v) (value)((uint64_t)(v) << 32 | SYMBOL_TAG)\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define GET_FIXNUM(v) ((uint64_t)(v) >> 3)\n");
    fprintf(fp, "#define GET_CLOSURE(v) ((struct closure *)((uint64_t)(v) & VALUE_MASK))\n");
    fprintf(fp, "#define GET_BOOL(v) ((uint64_t)(v) >> 4)\n");
    fprintf(fp, "#define GET_STRING(v) ((struct string *)((uint64_t)(v) & VALUE_MASK))\n");
    fprintf(fp, "#define GET_CHAR(v) ((char)((uint64_t)(v) >> 32))\n");
    fprintf(fp, "#define GET_SYMBOL(v) ((int)((uint64_t)(v) >> 32))\n");
    fprintf(fp, "\n");
    fprintf(fp, "#define IS_FIXNUM(v) (((uint64_t)(v) & TAG_MASK) == FIXNUM_TAG)\n");
    fprintf(fp, "#define IS_STRING(v) (((uint64_t)(v) & TAG_MASK) == STRING_TAG)\n");
    fprintf(fp, "#define IS_BOOL(v) (((uint64_t)(v) & BOOL_TAG_MASK) == BOOL_TAG)\n");
    fprintf(fp, "#define IS_VOID(v) (((uint64_t)(v) & VOID_TAG_MASK) == VOID_TAG)\n");
    fprintf(fp, "#define IS_CHAR(v) (((uint64_t)(v) & CHAR_TAG_MASK) == CHAR_TAG)\n");
    fprintf(fp, "#define IS_SYMBOL(v) (((uint64_t)(v) & SYMBOL_TAG_MASK) == SYMBOL_TAG)\n");
    fprintf(fp, "\n");

    fprintf(fp, "static struct symbol *symbols = NULL;\n");
    fprintf(fp, "\n");
    for (int i = 0; i < compiler->n_symbols; ++i) {
        fprintf(fp, "#define tok%.*s SYMBOL(%d)\n",
                compiler->reader->interned_mangled_len[compiler->symbols[i]],
                compiler->reader->interned_mangled[compiler->symbols[i]],
                i);
    }
    fprintf(fp, "\n");

    fprintf(fp, "static value envget(environment env, int index) {\n");
    fprintf(fp, "    value *vars = env;\n");
    fprintf(fp, "    return vars[index];\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value make_closure(funcptr func, int nargs, int nfreevars, ...) {\n");
    fprintf(fp, "    va_list args;\n");
    fprintf(fp, "    struct closure *closure = calloc(1, sizeof(struct closure) + nfreevars * sizeof(int));\n");
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
    fprintf(fp, "\n");
    fprintf(fp, "static value make_string(const char *s, size_t len) {\n");
    fprintf(fp, "    struct string *p = malloc(sizeof(struct string) + len);\n");
    fprintf(fp, "    p->len = len;\n");
    fprintf(fp, "    memcpy(p->s, s, len);\n");
    fprintf(fp, "    return STRING(p);\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");
    fprintf(fp, "static value display(value v) {\n");
    fprintf(fp, "    if (IS_FIXNUM(v)) {\n");
    fprintf(fp, "        printf(\"%%ld\", GET_FIXNUM(v));\n");
    fprintf(fp, "    } else if (IS_STRING(v)) {\n");
    fprintf(fp, "        printf(\"%%.*s\", (int) GET_STRING(v)->len, GET_STRING(v)->s);\n");
    fprintf(fp, "    } else if (IS_SYMBOL(v)) {\n");
    fprintf(fp, "        printf(\"%%.*s\", symbols[GET_SYMBOL(v)].name_len, symbols[GET_SYMBOL(v)].name);\n");
    fprintf(fp, "    } else if (IS_BOOL(v)) {\n");
    fprintf(fp, "        printf(\"%%s\", GET_BOOL(v) ? \"#t\" : \"#f\");\n");
    fprintf(fp, "    } else if (IS_VOID(v)) {\n");
    fprintf(fp, "        printf(\"#<void>\");\n");
    fprintf(fp, "    } else if (IS_CHAR(v)) {\n");
    fprintf(fp, "        char c = GET_CHAR(v);\n");
    fprintf(fp, "        switch (c) {\n");
    fprintf(fp, "        case '\\x07':\n");
    fprintf(fp, "            printf(\"#\\\\alarm\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\b':\n");
    fprintf(fp, "            printf(\"#\\\\backspace\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\x7f':\n");
    fprintf(fp, "            printf(\"#\\\\delete\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\x1b':\n");
    fprintf(fp, "            printf(\"#\\\\escape\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\n':\n");
    fprintf(fp, "            printf(\"#\\\\newline\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\0':\n");
    fprintf(fp, "            printf(\"#\\\\null\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\r':\n");
    fprintf(fp, "            printf(\"#\\\\return\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        case '\\t':\n");
    fprintf(fp, "            printf(\"#\\\\tab\");\n");
    fprintf(fp, "            break;\n");
    fprintf(fp, "        default:\n");
    fprintf(fp, "            if (c >= 32 || c < 127) {\n");
    fprintf(fp, "                printf(\"%%c\", c);\n");
    fprintf(fp, "            } else {\n");
    fprintf(fp, "                printf(\"#\\\\x%%02x\", c);\n");
    fprintf(fp, "            }\n");
    fprintf(fp, "        }\n");
    fprintf(fp, "    } else {\n");
    fprintf(fp, "        printf(\"#<object-%%p>\", v);\n");
    fprintf(fp, "    }\n");
    fprintf(fp, "\n");
    fprintf(fp, "    return VOID;\n");
    fprintf(fp, "}\n");
    fprintf(fp, "\n");

    /* add function prototypes */
    for (int i = 0; i < compiler->n_functions; ++i) {
        fprintf(fp, "static value %s(environment env, int nargs, ...);\n", compiler->functions[i]->name);
    }
    fprintf(fp, "\n");

    /* add function implementations */
    for (int i = 0; i < compiler->n_functions; ++i) {
        fprintf(fp, "static value %s(environment env, int nargs, ...) {\n", compiler->functions[i]->name);
        fwrite(compiler->functions[i]->code, 1, compiler->functions[i]->code_size, fp);
        if (compiler->functions[i]->varnum > 0) {
            fprintf(fp, "    return x%d;\n", compiler->functions[i]->varnum - 1);
        }
        fprintf(fp, "}\n\n");
    }

    fprintf(fp, "\n");
    fprintf(fp, "int main(int argc, const char *argv[]) {\n");

    fprintf(fp, "    symbols = malloc(sizeof(struct symbol) * %d);\n", compiler->n_symbols);
    for (int i = 0; i < compiler->n_symbols; ++i) {
        fprintf(fp, "    symbols[%d].name = \"%.*s\";\n", i,
                compiler->reader->interned_name_len[compiler->symbols[i]],
                compiler->reader->interned_name[compiler->symbols[i]]);
        fprintf(fp, "    symbols[%d].name_len = %d;\n", i, compiler->reader->interned_name_len[compiler->symbols[i]]);
    }
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

    struct reader *reader = create_reader(&lexer);

    read_token(&lexer);
    struct compiler compiler = {
        .reader = reader,
        .output_filename = "a.c",
    };
    compile_program(&compiler);

    return 0;
}
