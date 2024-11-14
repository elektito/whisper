#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef void *value;
typedef value *environment;
typedef struct closure *closure;
typedef void(*kont)(value v);
typedef value(*funcptr)(environment env, int nargs, ...);

struct closure {
    value (*func)(environment env, int nargs, ...);
    int n_args;
    int n_freevars;
    value freevars[];
};

struct pair {
    value car;
    value cdr;
};

struct string {
    size_t len;
    char *s;
};

struct symbol {
    size_t name_len;
    char *name;
};

enum object_type {
    OBJ_PORT,
};

enum port_direction {
    PORT_DIR_READ,
    PORT_DIR_WRITE,
};

struct object {
    enum object_type type;
    union {
        struct {
            int direction;
            int closed;
            FILE *fp;
            char *string;
            int64_t string_cap;
            int64_t string_len;
            value (*read_char)(value port);
            value (*peek_char)(value port);
            value (*read_line)(value port);
            void (*write_char)(value port, value ch);
            void (*printf)(value port, const char *fmt, ...);
        } port;
    };
};

#define FIXNUM_TAG 0x0
#define OBJECT_TAG 0x01
#define CLOSURE_TAG 0x02
#define STRING_TAG 0x03
#define PAIR_TAG 0x04
#define VOID_TAG 0x15    /*      10_101 */
#define BOOL_TAG 0xd     /*       1_101 */
#define TRUE_TAG 0x1d    /*      11_101 */
#define FALSE_TAG 0x0d   /*      01_101 */
#define CHAR_TAG 0x25    /*     100_101 */
#define SYMBOL_TAG 0x45  /*    1000_101 */
#define NIL_TAG 0x85     /*   10000_101 */
#define EOFOBJ_TAG 0x105 /* 100000_101 */

#define TAG_MASK 0x7
#define VALUE_MASK 0xfffffffffffffff8
#define BOOL_TAG_MASK 0xf
#define VOID_TAG_MASK 0x1f
#define CHAR_TAG_MASK 0x3f
#define SYMBOL_TAG_MASK 0x7f
#define EOFOBJ_TAG_MASK 0x1ff

#define FIXNUM(v) (value)((uint64_t)(v) << 3 | FIXNUM_TAG)
#define CLOSURE(v) (value)((uint64_t)(v) | CLOSURE_TAG)
#define PAIR(v) (value)((uint64_t)(v) | PAIR_TAG)
#define STRING(v) (value)((uint64_t)(v) | STRING_TAG)
#define VOID (value)(VOID_TAG)
#define TRUE (value)(TRUE_TAG)
#define FALSE (value)(FALSE_TAG)
#define BOOL(v) ((v) ? TRUE : FALSE)
#define CHAR(v) (value)((uint64_t)(v) << 32 | CHAR_TAG)
#define SYMBOL(v) (value)((uint64_t)(v) << 32 | SYMBOL_TAG)
#define NIL (value)(NIL_TAG)
#define EOFOBJ (value)(EOFOBJ_TAG)
#define OBJECT(v) (value)((uint64_t)(v) | OBJECT_TAG)

#define GET_FIXNUM(v) ((int64_t)(v) >> 3)
#define GET_CLOSURE(v) ((struct closure *)((uint64_t)(v) & VALUE_MASK))
#define GET_BOOL(v) ((uint64_t)(v) >> 4)
#define GET_STRING(v) ((struct string *)((uint64_t)(v) & VALUE_MASK))
#define GET_PAIR(v) ((struct pair *)((uint64_t)(v) & VALUE_MASK))
#define GET_CHAR(v) ((char)((uint64_t)(v) >> 32))
#define GET_SYMBOL(v) ((int)((uint64_t)(v) >> 32))
#define GET_OBJECT(v) ((struct object *)((uint64_t)(v) & VALUE_MASK))

#define IS_FIXNUM(v) (((uint64_t)(v) & TAG_MASK) == FIXNUM_TAG)
#define IS_CLOSURE(v) (((uint64_t)(v) & TAG_MASK) == CLOSURE_TAG)
#define IS_STRING(v) (((uint64_t)(v) & TAG_MASK) == STRING_TAG)
#define IS_BOOL(v) (((uint64_t)(v) & BOOL_TAG_MASK) == BOOL_TAG)
#define IS_VOID(v) (((uint64_t)(v) & VOID_TAG_MASK) == VOID_TAG)
#define IS_CHAR(v) (((uint64_t)(v) & CHAR_TAG_MASK) == CHAR_TAG)
#define IS_SYMBOL(v) (((uint64_t)(v) & SYMBOL_TAG_MASK) == SYMBOL_TAG)
#define IS_NIL(v) ((uint64_t)(v) == NIL_TAG)
#define IS_PAIR(v) (((uint64_t)(v) & TAG_MASK) == PAIR_TAG)
#define IS_EOFOBJ(v) ((uint64_t)(v) == EOFOBJ_TAG)
#define IS_OBJECT(v) (((uint64_t)(v) & TAG_MASK) == OBJECT_TAG)
#define IS_PORT(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_PORT)

#define RAISE(...) { fprintf(stderr, "exception: " __VA_ARGS__); fprintf(stderr, "\n"); cleanup(); exit(1); }

static int n_symbols = 0;
static struct symbol *symbols = NULL;

static struct object current_input_port;
static struct object current_output_port;
static struct object current_error_port;

static int cmdline_argc;
static const char **cmdline_argv;

static void cleanup(void) {}

static value envget(environment env, int index) {
    value *vars = env;
    return vars[index];
}

static value make_closure(funcptr func, int nargs, int nfreevars, ...) {
    va_list args;
    struct closure *closure = calloc(1, sizeof(struct closure) + nfreevars * sizeof(value));
    closure->func = func;
    closure->n_args = nargs;
    closure->n_freevars = nfreevars;
    va_start(args, nfreevars);
    for (int i = 0; i < nfreevars; ++i) {
        closure->freevars[i] = va_arg(args, value);
    };
    va_end(args);
    return CLOSURE(closure);
}

static value make_pair(value car, value cdr) {
    struct pair *pair = malloc(sizeof(struct pair));
    pair->car = car;
    pair->cdr = cdr;
    return PAIR(pair);
}
static value reverse_list(value list, value acc) {
    if (list == NIL) {
        return acc;
    } else {
        struct pair *p = GET_PAIR(list);
        return reverse_list(p->cdr, make_pair(p->car, acc));
    }
}

static value make_string(const char *s, size_t len) {
    struct string *p = malloc(sizeof(struct string));
    p->s = malloc(len + 1);
    p->len = len;
    memcpy(p->s, s, len);
    p->s[len] = 0;
    return STRING(p);
}

static value file_read_line(value port) {
    FILE *fp = GET_OBJECT(port)->port.fp;
    char buf[256];
    char *r = fgets(buf, sizeof(buf), fp);
    if (!r) {
        if (feof(fp)) { return EOFOBJ; };
        RAISE("cannot read from file: %s", strerror(errno));
    }

    size_t len = strlen(buf);
    if (buf[len-1] == '\n') len--;
    struct string *str = GET_STRING(make_string(buf, len));

    while (len == sizeof(buf) - 1) {
        r = fgets(buf, sizeof(buf), fp);
        if (!r && !feof(fp)) {
            RAISE("cannot read from file: %s", strerror(errno));
        }

        len = strlen(buf);
        if (buf[len-1] == '\n') len--;
        str->s = realloc(str->s, str->len + len + 1);
        memcpy(str->s + str->len, buf, len + 1);
        str->len += len;
    }

    return STRING(str);
}

static value file_read_char(value port) {
    FILE *fp = GET_OBJECT(port)->port.fp;
    char ch = getc(fp);
    if (ch == EOF) return EOFOBJ;
    return CHAR(ch);
}

static value file_peek_char(value port) {
    FILE *fp = GET_OBJECT(port)->port.fp;
    char ch = getc(fp);
    if (ch == EOF) return EOFOBJ;
    ungetc(ch, fp);
    return CHAR(ch);
}

static void file_write_char(value port, value ch) {
    FILE *fp = GET_OBJECT(port)->port.fp;
    int ret = putc(GET_CHAR(ch), fp);
    if (ret == EOF) { RAISE("error writing to file: %s", strerror(errno)); }
}

static void file_printf(value port, const char *fmt, ...) {
    FILE *fp = GET_OBJECT(port)->port.fp;
    va_list args;
    va_start(args, fmt);
    vfprintf(fp, fmt, args);
    va_end(args);
}

static void string_write_char(value port, value ch) {
    int64_t needed = GET_OBJECT(port)->port.string_cap + 1;
    if (needed > GET_OBJECT(port)->port.string_cap) {
        GET_OBJECT(port)->port.string_cap *= 2;
        GET_OBJECT(port)->port.string = realloc(GET_OBJECT(port)->port.string, GET_OBJECT(port)->port.string_cap);
    }

    GET_OBJECT(port)->port.string[GET_OBJECT(port)->port.string_len] = GET_CHAR(ch);
    GET_OBJECT(port)->port.string_len++;
}

static void string_printf(value port, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    int extra_needed = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    struct object *op = GET_OBJECT(port);
    int64_t total_needed = op->port.string_cap + extra_needed;
    if (total_needed > op->port.string_cap) {
        while (op->port.string_cap < total_needed) op->port.string_cap *= 2;
        op->port.string = realloc(op->port.string, op->port.string_cap);
    }

    va_start(args, fmt);
    vsnprintf(op->port.string + op->port.string_len, op->port.string_cap - op->port.string_len, fmt, args);
    op->port.string_len += extra_needed;
    va_end(args);
}

static void _display(value v, value port);
static void _display_pair(struct pair *v, value port, int in_the_middle) {
    if (!in_the_middle) GET_OBJECT(port)->port.printf(port, "(");
    _display(v->car, port);
    if (IS_NIL(v->cdr)) {
        GET_OBJECT(port)->port.printf(port, ")");
    } else if (IS_PAIR(v->cdr)) {
        GET_OBJECT(port)->port.printf(port, " ");
        _display_pair(GET_PAIR(v->cdr), port, 1);
    } else {
        GET_OBJECT(port)->port.printf(port, " . ");
        _display(v->cdr, port);
        GET_OBJECT(port)->port.printf(port, ")");
    }
}

static void _display(value v, value port) {
    if (IS_FIXNUM(v)) {
        GET_OBJECT(port)->port.printf(port, "%ld", GET_FIXNUM(v));
    } else if (IS_STRING(v)) {
        GET_OBJECT(port)->port.printf(port, "%.*s", (int) GET_STRING(v)->len, GET_STRING(v)->s);
    } else if (IS_SYMBOL(v)) {
        GET_OBJECT(port)->port.printf(port, "%.*s", (int) symbols[GET_SYMBOL(v)].name_len, symbols[GET_SYMBOL(v)].name);
    } else if (IS_BOOL(v)) {
        GET_OBJECT(port)->port.printf(port, "%s", GET_BOOL(v) ? "#t" : "#f");
    } else if (IS_VOID(v)) {
        GET_OBJECT(port)->port.printf(port, "#<void>");
    } else if (IS_CHAR(v)) {
        GET_OBJECT(port)->port.printf(port, "%c", GET_CHAR(v));
    } else if (IS_NIL(v)) {
        GET_OBJECT(port)->port.printf(port, "()");
    } else if (IS_PAIR(v)) {
        _display_pair(GET_PAIR(v), port, 0);
    } else if (IS_CLOSURE(v)) {
        GET_OBJECT(port)->port.printf(port, "#<procedure-%d>", GET_CLOSURE(v)->n_args);
    } else if (IS_EOFOBJ(v)) {
        GET_OBJECT(port)->port.printf(port, "#<eof-object>");
    } else {
        GET_OBJECT(port)->port.printf(port, "#<object-%p>", v);
    }
}

static void _write(value v, value port);
static void _write_pair(struct pair *v, value port, int in_the_middle) {
    if (!in_the_middle) GET_OBJECT(port)->port.printf(port, "(");
    _write(v->car, port);
    if (IS_NIL(v->cdr)) {
        GET_OBJECT(port)->port.printf(port, ")");
    } else if (IS_PAIR(v->cdr)) {
        GET_OBJECT(port)->port.printf(port, " ");
        _write_pair(GET_PAIR(v->cdr), port, 1);
    } else {
        GET_OBJECT(port)->port.printf(port, " . ");
        _write(v->cdr, port);
        GET_OBJECT(port)->port.printf(port, ")");
    }
}

static void _write_char_literal(value v, value port) {
    char ch = GET_CHAR(v);
    char buf[16];
    char *text;
    switch (ch) {
    case '\a':
        text = "#\\alarm";
        break;
    case '\b':
        text = "#\\backspace";
        break;
    case '\x7f':
        text = "#\\delete";
        break;
    case '\x1b':
        text = "#\\escape";
        break;
    case '\n':
        text = "#\\newline";
        break;
    case '\0':
        text = "#\\null";
        break;
    case '\r':
        text = "#\\return";
        break;
    case ' ':
        text = "#\\space";
        break;
    case '\t':
        text = "#\\tab";
        break;
    default:
        if (ch >= 32 && ch < 127)
            sprintf(buf, "#\\%c", ch);
        else
            sprintf(buf, "#\\x%02x", (int) ch);
        text = buf;
    }

    GET_OBJECT(port)->port.printf(port, "%s", text);
}

static void _write_string_literal(value v, value port) {
    struct object *op = GET_OBJECT(port);
    struct string *s = GET_STRING(v);
    op->port.printf(port, "\"");
    for (int i = 0; i < s->len; ++i) {
        switch (s->s[i]) {
        case '\a':
            op->port.printf(port, "\\a");
            break;
        case '\b':
            op->port.printf(port, "\\b");
            break;
        case '\r':
            op->port.printf(port, "\\r");
            break;
        case '\n':
            op->port.printf(port, "\\n");
            break;
        case '\t':
            op->port.printf(port, "\\t");
            break;
        case '"':
            op->port.printf(port, "\\\"");
            break;
        case '\\':
            op->port.printf(port, "\\\\");
            break;
        default:
            if (s->s[i] >= 32 && s->s[i] < 127)
                op->port.printf(port, "%c", s->s[i]);
            else
                op->port.printf(port, "\\x%02x", (int) s->s[i]);
        }
    }
    op->port.printf(port, "\"");
}

static void _write(value v, value port) {
    if (IS_FIXNUM(v)) {
        GET_OBJECT(port)->port.printf(port, "%ld", GET_FIXNUM(v));
    } else if (IS_STRING(v)) {
        _write_string_literal(v, port);
    } else if (IS_SYMBOL(v)) {
        GET_OBJECT(port)->port.printf(port, "%.*s", (int) symbols[GET_SYMBOL(v)].name_len, symbols[GET_SYMBOL(v)].name);
    } else if (IS_BOOL(v)) {
        GET_OBJECT(port)->port.printf(port, "%s", GET_BOOL(v) ? "#t" : "#f");
    } else if (IS_VOID(v)) {
        GET_OBJECT(port)->port.printf(port, "#<void>");
    } else if (IS_CHAR(v)) {
        _write_char_literal(v, port);
    } else if (IS_NIL(v)) {
        GET_OBJECT(port)->port.printf(port, "()");
    } else if (IS_PAIR(v)) {
        _write_pair(GET_PAIR(v), port, 0);
    } else if (IS_CLOSURE(v)) {
        GET_OBJECT(port)->port.printf(port, "#<procedure-%d>", GET_CLOSURE(v)->n_args);
    } else if (IS_EOFOBJ(v)) {
        GET_OBJECT(port)->port.printf(port, "#<eof-object>");
    } else {
        GET_OBJECT(port)->port.printf(port, "#<object-%p>", v);
    }
}

static value string_to_symbol(value v) {
    for (int i = 0; i < n_symbols; ++i) {
        if (symbols[i].name_len == GET_STRING(v)->len && memcmp(symbols[i].name, GET_STRING(v)->s, symbols[i].name_len) == 0) {
            return SYMBOL(i);
        }
    }

    n_symbols++;
    symbols = realloc(symbols, n_symbols * sizeof(struct symbol));
    symbols[n_symbols - 1].name_len = GET_STRING(v)->len;
    symbols[n_symbols - 1].name = malloc(GET_STRING(v)->len);
    memcpy(symbols[n_symbols - 1].name, GET_STRING(v)->s, GET_STRING(v)->len);
    return SYMBOL(n_symbols - 1);
}

static value alloc_string(size_t len, char fill) {
    struct string *str = malloc(sizeof(struct string));
    str->len = len;
    str->s = malloc(len);
    memset(str->s, fill, len);
    return STRING(str);
}

static int string_cmp(struct string *s1, struct string *s2) {
    size_t min_len = s1->len < s2->len ? s1->len : s2->len;
    int cmp = memcmp(s1->s, s2->s, min_len);
    if (cmp != 0) return cmp;
    if (s1->len < s2->len) {
        return -1;
    } else if (s1->len > s2->len) {
        return 1;
    } else {
        return 0;
    }
}

/************ primcall functions ***********/
static value primcall_car(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("car needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value arg = va_arg(args, value);
    va_end(args);
    if (!IS_PAIR(arg)) { RAISE("car argument is not a pair") }
    return GET_PAIR(arg)->car;
}

static value primcall_cdr(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("car needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value arg = va_arg(args, value);
    va_end(args);
    if (!IS_PAIR(arg)) { RAISE("cdr argument is not a pair") }
    return GET_PAIR(arg)->cdr;
}

static value primcall_char_downcase(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("char-downcase needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value ch = va_arg(args, value);
    va_end(args);
    if (!IS_CHAR(ch)) { RAISE("char-downcase argument is not a char") }
    return GET_CHAR(ch) >= 'A' && GET_CHAR(ch) <= 'Z' ? CHAR(GET_CHAR(ch) - 'A' + 'a') : ch;
}

static value primcall_char_upcase(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("char-upcase needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value ch = va_arg(args, value);
    va_end(args);
    if (!IS_CHAR(ch)) { RAISE("char-upcase argument is not a char") }
    return GET_CHAR(ch) >= 'a' && GET_CHAR(ch) <= 'z' ? CHAR(GET_CHAR(ch) - 'a' + 'A') : ch;
}

static value primcall_char_to_integer(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("char->integer needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value ch = va_arg(args, value);
    va_end(args);
    if (!IS_CHAR(ch)) { RAISE("char->integer argument is not a char") }
    return FIXNUM((int) GET_CHAR(ch));
}

static value primcall_char_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("char? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value x = va_arg(args, value);
    va_end(args);
    return BOOL(IS_CHAR(x));
}

static value primcall_close_port(environment env, int nargs, ...) {
    va_list args;
    va_start(args, nargs);
    value port = va_arg(args, value);
    va_end(args);
    if (!IS_PORT(port)) { RAISE("close-port argument is not a port") }
    if (GET_OBJECT(port)->port.closed) return VOID;
    int ret = fclose(GET_OBJECT(port)->port.fp);
    if (ret) { RAISE("failed to close the port: %s", strerror(errno)); }
    GET_OBJECT(port)->port.closed = 1;
    return VOID;
}

static value primcall_cons(environment env, int nargs, ...) {
    if (nargs != 2) { RAISE("cons needs two arguments"); }
    va_list args;
    va_start(args, nargs);
    value car = va_arg(args, value);
    value cdr = va_arg(args, value);
    va_end(args);
    return make_pair(car, cdr);
}

static value primcall_command_line(environment env, int nargs, ...) {
    if (nargs != 0) { RAISE("cons needs two arguments"); }

    value cmdline = NIL;
    for (int i = cmdline_argc - 1; i >= 0; --i) {
        value s = make_string(cmdline_argv[i], strlen(cmdline_argv[i]));
        cmdline = make_pair(s, cmdline);
    }

    return cmdline;
}

static value primcall_current_error_port(environment env, int nargs, ...) {
    if (nargs != 0) { RAISE("current-error-port needs no arguments"); }
    return OBJECT(&current_error_port);
}

static value primcall_current_input_port(environment env, int nargs, ...) {
    if (nargs != 0) { RAISE("current-input-port needs no arguments"); }
    return OBJECT(&current_input_port);
}

static value primcall_current_output_port(environment env, int nargs, ...) {
    if (nargs != 0) { RAISE("current-output-port needs no arguments"); }
    return OBJECT(&current_output_port);
}

static value primcall_display(environment env, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("display needs one or two arguments"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    value port = nargs == 1 ? OBJECT(&current_output_port) : va_arg(args, value);
    va_end(args);
    if (!IS_PORT(port)) { RAISE("writing to non-port"); }
    if (GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("writing to non-output port"); }
    _display(v, port);
    return VOID;
}

static value primcall_eof_object_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("eof-object? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    va_end(args);
    return BOOL(IS_EOFOBJ(v));
}

static value primcall_eq_q(environment env, int nargs, ...) {
    if (nargs != 2) { RAISE("eq? needs two arguments"); }
    va_list args;
    va_start(args, nargs);
    value v1 = va_arg(args, value);
    value v2 = va_arg(args, value);
    va_end(args);
    return BOOL(v1 == v2);
}

static value primcall_get_output_string(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("get-output-string needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value port = va_arg(args, value);
    va_end(args);
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE || GET_OBJECT(port)->port.string == NULL) { RAISE("argument is not an output string port"); }
    return make_string(GET_OBJECT(port)->port.string, GET_OBJECT(port)->port.string_len);
}

static value primcall_input_port_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("input-port? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    va_end(args);
    return BOOL(IS_PORT(v) && GET_OBJECT(v)->port.direction == PORT_DIR_READ);
}

static value primcall_integer_to_char(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("integer->char needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    va_end(args);
    if (!IS_FIXNUM(n)) { RAISE("integer->char argument is not a number") }
    if (GET_FIXNUM(n) < 0 || GET_FIXNUM(n) > 255) { RAISE("integer->char argument is out of range") }
    return CHAR((char) GET_FIXNUM(n));
}

static value primcall_make_string(environment env, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("make-string needs one or two arguments"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    value ch = nargs == 1 ? CHAR(0) : va_arg(args, value);
    va_end(args);
    if (!IS_FIXNUM(n)) { RAISE("make-string first argument should be a number"); }
    if (!IS_CHAR(ch)) { RAISE("make-string second argument should be a character"); }
    return alloc_string(GET_FIXNUM(n), GET_CHAR(ch));
}

static value primcall_newline(environment env, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("newline needs zero or one argument"); }
    va_list args;
    va_start(args, nargs);
    value port = nargs == 1 ? va_arg(args, value) : OBJECT(&current_output_port);
    va_end(args);
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("newline argument is not an output port"); }
    GET_OBJECT(port)->port.write_char(port, CHAR('\n'));
    return VOID;
}

static value primcall_number_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("number? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    va_end(args);
    return BOOL(IS_FIXNUM(v));
}

static value primcall_number_to_string(environment env, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("number->string needs one or two arguments"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    value base = nargs == 1 ? FIXNUM(10) : va_arg(args, value);
    va_end(args);
    if (!IS_FIXNUM(n)) { RAISE("number->string first argument should be a number"); }
    if (!IS_FIXNUM(base)) { RAISE("number->string second argument should be a number"); }
    char buf[128];
    int start = 0;
    int64_t m = GET_FIXNUM(n);
    if (m < 0) { buf[0] = '-'; start = 1; m = -m; }
    if (base == FIXNUM(10))
        snprintf(buf + start, sizeof(buf), "%ld", m);
    else if (base == FIXNUM(16))
        snprintf(buf + start, sizeof(buf), "%lx", m);
    else if (base == FIXNUM(8))
        snprintf(buf + start, sizeof(buf), "%lo", m);
    else if (base == FIXNUM(2)) {
        while (m >= 2) { buf[start++] = '0' + (m % 2); m /= 2; }
        buf[start++] = '0' + m;
        buf[start] = 0;
    } else
        RAISE("radix not supported by number->string");
    return make_string(buf, strlen(buf));
}

static value primcall_open_input_file(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("open-input-file needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value filename = va_arg(args, value);
    va_end(args);
    if (!IS_STRING(filename)) { RAISE("filename is not a string"); }
    int filename_len = GET_STRING(filename)->len;
    char *filenamez = malloc(filename_len + 1);
    snprintf(filenamez, filename_len + 1, "%.*s", filename_len, GET_STRING(filename)->s);
    FILE *fp = fopen(filenamez, "r");
    if (!fp) { RAISE("error opening file: %s", strerror(errno)); }
    struct object *obj = calloc(1, sizeof(struct object));
    obj->type = OBJ_PORT;
    obj->port.direction = PORT_DIR_READ;
    obj->port.fp = fp;
    obj->port.read_char = file_read_char;
    obj->port.peek_char = file_peek_char;
    obj->port.read_line = file_read_line;
    return OBJECT(obj);
}

static value primcall_open_output_file(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("open-output-file needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value filename = va_arg(args, value);
    va_end(args);
    if (!IS_STRING(filename)) { RAISE("filename is not a string"); }
    int filename_len = GET_STRING(filename)->len;
    char *filenamez = malloc(filename_len + 1);
    snprintf(filenamez, filename_len + 1, "%.*s", filename_len, GET_STRING(filename)->s);
    FILE *fp = fopen(filenamez, "w");
    if (!fp) { RAISE("error opening file: %s", strerror(errno)); }
    struct object *obj = calloc(1, sizeof(struct object));
    obj->type = OBJ_PORT;
    obj->port.direction = PORT_DIR_WRITE;
    obj->port.fp = fp;
    obj->port.printf = file_printf;
    obj->port.write_char = file_write_char;
    return OBJECT(obj);
}

static value primcall_open_output_string(environment env, int nargs, ...) {
    if (nargs != 0) { RAISE("open-output-string accepts no arguments"); }
    struct object *obj = calloc(1, sizeof(struct object));
    obj->type = OBJ_PORT;
    obj->port.direction = PORT_DIR_WRITE;
    obj->port.string = malloc(128);
    obj->port.string_cap = 128;
    obj->port.string_len = 0;
    obj->port.printf = string_printf;
    obj->port.write_char = string_write_char;
    return OBJECT(obj);
}

static value primcall_pair_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("port? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value x = va_arg(args, value);
    va_end(args);
    return BOOL(IS_PAIR(x));
}

static value primcall_peek_char(environment env, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("peek-char needs zero or one argument"); }
    va_list args;
    va_start(args, nargs);
    value port = nargs == 1 ? va_arg(args, value) : OBJECT(&current_input_port);
    va_end(args);
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE("peek-char argument is not an input port"); }
    return GET_OBJECT(port)->port.peek_char(port);
}

static value primcall_port_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("port? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    va_end(args);
    return BOOL(IS_PORT(v));
}

static value primcall_read_char(environment env, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("read-char needs zero or one argument"); }
    va_list args;
    va_start(args, nargs);
    value port = nargs == 1 ? va_arg(args, value) : OBJECT(&current_input_port);
    va_end(args);
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE("read-char argument is not an input port"); }
    return GET_OBJECT(port)->port.read_char(port);
}

static value primcall_read_line(environment env, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("read-line needs zero or one argument"); }
    va_list args;
    va_start(args, nargs);
    value port = nargs == 1 ? va_arg(args, value) : OBJECT(&current_input_port);
    va_end(args);
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE("read-line argument is not an input port"); }
    return GET_OBJECT(port)->port.read_line(port);
}

static value primcall_string_to_number(environment env, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("string-to-number needs one or two arguments"); }
    va_list args;
    va_start(args, nargs);
    value str_v = va_arg(args, value);
    value base = nargs == 1 ? FIXNUM(10) : va_arg(args, value);
    va_end(args);
    if (!IS_STRING(str_v)) { RAISE("string->number first argument must be a string"); }
    if (!IS_FIXNUM(base)) { RAISE("string->number second argument must be a number"); }
    if (GET_STRING(str_v)->len == 0) return FALSE;

    /* create a zero terminated version of the string for strtoll */
    char *str = malloc(GET_STRING(str_v)->len + 1);
    memcpy(str, GET_STRING(str_v)->s, GET_STRING(str_v)->len);
    str[GET_STRING(str_v)->len] = 0;

    char *endptr;
    int64_t result = strtoll(str, &endptr, GET_FIXNUM(base));
    if (endptr != str + GET_STRING(str_v)->len) { free(str); return FALSE; }
    free(str);
    return FIXNUM(result);
}

static value primcall_string_to_symbol(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("string->symbol needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value str = va_arg(args, value);
    va_end(args);
    if (!IS_STRING(str)) { RAISE("string->symbol argument is not a string"); }
    return string_to_symbol(str);
}

static value primcall_string_append(environment env, int nargs, ...) {
    int total_size = 0;
    va_list args;
    va_start(args, nargs);
    for (int i = 0; i < nargs; ++i) {
        struct string *str = GET_STRING(va_arg(args, value));
        total_size += str->len;
    }
    va_end(args);

    struct string *concat = malloc(sizeof(struct string));
    concat->len = total_size;
    concat->s = malloc(total_size);

    va_start(args, nargs);
    size_t offset = 0;
    for (int i = 0; i < nargs; ++i) {
        struct string *str = GET_STRING(va_arg(args, value));
        memcpy(concat->s + offset, str->s, str->len);
        offset += str->len;
    }
    va_end(args);

    return STRING(concat);
}

static value primcall_string_copy(environment env, int nargs, ...) {
    if (nargs > 3) { RAISE("string-copy needs at most three arguments"); }
    va_list args;
    va_start(args, nargs);
    value str = va_arg(args, value);
    if (!IS_STRING(str)) { RAISE("string-copy first argument is not a string"); }
    value start = nargs > 1 ? va_arg(args, value) : FIXNUM(0);
    value end = nargs > 2 ? va_arg(args, value) : FIXNUM(GET_STRING(str)->len);
    va_end(args);
    if (!IS_FIXNUM(start)) { RAISE("string-copy second argument is not a number"); }
    if (!IS_FIXNUM(end)) { RAISE("string-copy third argument is not a number"); }
    if (GET_FIXNUM(start) < 0 || GET_FIXNUM(start) >= GET_STRING(str)->len) { RAISE("string-copy start index is out of range"); }
    if (GET_FIXNUM(end) < 0 || GET_FIXNUM(end) > GET_STRING(str)->len) { RAISE("string-copy end index is out of range"); }
    struct string *result = calloc(1, sizeof(struct string));
    result->len = GET_FIXNUM(end) - GET_FIXNUM(start);
    result->s = malloc(result->len);
    memcpy(result->s, GET_STRING(str)->s + GET_FIXNUM(start), result->len);
    return STRING(result);
}

static value primcall_string_length(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("string-length needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value str = va_arg(args, value);
    va_end(args);
    if (!IS_STRING(str)) { RAISE("string-length argument is not a string"); }
    return FIXNUM(GET_STRING(str)->len);
}

static value primcall_string_ref(environment env, int nargs, ...) {
    if (nargs != 2) { RAISE("string-ref needs two arguments"); }
    va_list args;
    va_start(args, nargs);
    value str = va_arg(args, value);
    value idx = va_arg(args, value);
    va_end(args);
    if (!IS_STRING(str)) { RAISE("string-ref first argument is not a string"); }
    if (!IS_FIXNUM(idx)) { RAISE("string-ref second argument is not a number"); }
    if (GET_FIXNUM(idx) < 0 || GET_FIXNUM(idx) >= GET_STRING(str)->len) { RAISE("string-ref index is out of range"); }
    return CHAR(GET_STRING(str)->s[GET_FIXNUM(idx)]);
}

static value primcall_string_eq_q(environment env, int nargs, ...) {
    if (nargs == 0) { RAISE("string=? needs at least one argument"); }
    if (nargs == 1) return TRUE;
    va_list args;
    va_start(args, nargs);
    value prev = va_arg(args, value);
    for (int i = 1; i < nargs; ++i) {
        value cur = va_arg(args, value);
        if (string_cmp(GET_STRING(prev), GET_STRING(cur)) != 0) {
            va_end(args);
            return FALSE;
        }
        prev = cur;
    }

    va_end(args);
    return TRUE;
}

static value primcall_string_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("string? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    va_end(args);
    return BOOL(IS_STRING(v));
}

static value primcall_substring(environment env, int nargs, ...) {
    if (nargs != 3) { RAISE("substring needs three arguments"); }
    va_list args;
    va_start(args, nargs);
    value str = va_arg(args, value);
    value start = va_arg(args, value);
    value end = va_arg(args, value);
    va_end(args);
    if (!IS_STRING(str)) { RAISE("substring first argument is not a string"); }
    if (!IS_FIXNUM(start)) { RAISE("substring second argument is not a number"); }
    if (!IS_FIXNUM(end)) { RAISE("substring third argument is not a number"); }
    if (GET_FIXNUM(start) < 0 || GET_FIXNUM(start) >= GET_STRING(str)->len) { RAISE("substring start index is out of range"); }
    if (GET_FIXNUM(end) < 0 || GET_FIXNUM(end) > GET_STRING(str)->len) { RAISE("substring end index is out of range"); }
    struct string *result = calloc(1, sizeof(struct string));
    result->len = GET_FIXNUM(end) - GET_FIXNUM(start);
    result->s = malloc(result->len);
    memcpy(result->s, GET_STRING(str)->s + GET_FIXNUM(start), result->len);
    return STRING(result);
}

static value primcall_symbol_q(environment env, int nargs, ...) {
    if (nargs != 1) { RAISE("symbol? needs a single argument"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    va_end(args);
    return BOOL(IS_SYMBOL(v));
}

static value primcall_write(environment env, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("write needs one or two arguments"); }
    va_list args;
    va_start(args, nargs);
    value v = va_arg(args, value);
    value port = nargs == 1 ? OBJECT(&current_output_port) : va_arg(args, value);
    va_end(args);
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("write second argument is not an output port"); }
    _write(v, port);
    return VOID;
}

static value primcall_write_char(environment env, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("write-char needs one or two arguments"); }
    va_list args;
    va_start(args, nargs);
    value ch = va_arg(args, value);
    value port = nargs == 1 ? OBJECT(&current_output_port) : va_arg(args, value);
    va_end(args);
    if (!IS_CHAR(ch)) { RAISE("write-char first argument is not a char"); }
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("write-char second argument is not an output port"); }
    GET_OBJECT(port)->port.write_char(port, ch);
    return VOID;
}

static value primcall_add(environment env, int nargs, ...) {
    value result = FIXNUM(0);
    va_list args;
    va_start(args, nargs);
    for (int i = 0; i < nargs; ++i) {
        value v = va_arg(args, value);
        if (!IS_FIXNUM(v)) { RAISE("addition (+) argument is not a number") }
        result += (int64_t) v;
    }
    return result;
}

static value primcall_div(environment env, int nargs, ...) {
    if (nargs < 1) { RAISE("division (-) needs at least one argument"); }
    va_list args;
    va_start(args, nargs);
    value result_v = va_arg(args, value);
    if (!IS_FIXNUM(result_v)) { RAISE("division (/) argument is not a number"); }
    int64_t result = GET_FIXNUM(result_v);
    if (nargs == 1) {
        if (result == 1)
            return FIXNUM(1);
        if (result == -1)
            return FIXNUM(-1);
        if (result == 0)
            RAISE("division by zero");
        return FIXNUM(0); /* we don't have fractionals, so 1/n is always zero */
    }
    for (int i = 1; i < nargs; ++i) {
        value v = va_arg(args, value);
        if (!IS_FIXNUM(v)) { RAISE("division (/) argument is not a number") }
        if (GET_FIXNUM(v) == 0)
            RAISE("division by zero");
        result /= GET_FIXNUM(v);
    }
    return FIXNUM(result);
}

static value primcall_mul(environment env, int nargs, ...) {
    int64_t result = 1;
    va_list args;
    va_start(args, nargs);
    for (int i = 0; i < nargs; ++i) {
        value v = va_arg(args, value);
        if (!IS_FIXNUM(v)) { RAISE("multiplication (*) argument is not a number") }
        result *= GET_FIXNUM(v);
    }
    return FIXNUM(result);
}

static value primcall_sub(environment env, int nargs, ...) {
    if (nargs < 1) { RAISE("subtraction (-) needs at least one argument"); }
    va_list args;
    va_start(args, nargs);
    value result = va_arg(args, value);
    if (!IS_FIXNUM(result)) { RAISE("subtraction (-) argument is not a number"); }
    if (nargs == 1) return FIXNUM(-GET_FIXNUM(result));
    for (int i = 1; i < nargs; ++i) {
        value v = va_arg(args, value);
        if (!IS_FIXNUM(v)) { RAISE("subtraction (-) argument is not a number") }
        result -= (int64_t) v;
    }
    return result;
}

static value primcall_num_eq(environment env, int nargs, ...) {
    if (nargs < 1) { RAISE("= needs at least one argument"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    if (!IS_FIXNUM(n)) { RAISE("= argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = va_arg(args, value);
        if (!IS_FIXNUM(m)) { RAISE("= argument is not a number") }
        if (n != m) return FALSE;
    }
    return TRUE;
}

static value primcall_num_lt(environment env, int nargs, ...) {
    if (nargs < 1) { RAISE("< needs at least one argument"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    if (!IS_FIXNUM(n)) { RAISE("< argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = va_arg(args, value);
        if (!IS_FIXNUM(m)) { RAISE("< argument is not a number") }
        if (n >= m) return FALSE;
    }
    return TRUE;
}

static value primcall_num_gt(environment env, int nargs, ...) {
    if (nargs < 1) { RAISE("> needs at least one argument"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    if (!IS_FIXNUM(n)) { RAISE("> argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = va_arg(args, value);
        if (!IS_FIXNUM(m)) { RAISE("> argument is not a number") }
        if (n <= m) return FALSE;
    }
    return TRUE;
}

static value primcall_num_le(environment env, int nargs, ...) {
    if (nargs < 1) { RAISE("<= needs at least one argument"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    if (!IS_FIXNUM(n)) { RAISE("<= argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = va_arg(args, value);
        if (!IS_FIXNUM(m)) { RAISE("<= argument is not a number") }
        if (n > m) return FALSE;
    }
    return TRUE;
}

static value primcall_num_ge(environment env, int nargs, ...) {
    if (nargs < 1) { RAISE(">= needs at least one argument"); }
    va_list args;
    va_start(args, nargs);
    value n = va_arg(args, value);
    if (!IS_FIXNUM(n)) { RAISE(">= argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = va_arg(args, value);
        if (!IS_FIXNUM(m)) { RAISE(">= argument is not a number") }
        if (n < m) return FALSE;
    }
    return TRUE;
}