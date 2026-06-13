#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define MAX_ARGS INT_MAX

enum call_flags {
    NO_CALL_FLAGS = 0,
    CALL_HAS_ARG_ARRAY = 1,
};

typedef void *value;
typedef value *environment;
typedef struct closure *closure;
typedef void(*kont)(value v);
typedef value(*funcptr)(environment env, enum call_flags flags, int nargs, ...);

/************ tags and masks and some macros ***********/

#define FIXNUM_TAG 0x0
#define OBJECT_TAG 0x01
#define CLOSURE_TAG 0x02
#define STRING_TAG 0x03
#define PAIR_TAG 0x04
#define SENTINEL_TAG 0x5       /*  0...0_101 */
#define VOID_TAG 0x15          /*     10_101 */
#define BOOL_TAG 0xd           /*      1_101 */
#define TRUE_TAG 0x1d          /*     11_101 */
#define FALSE_TAG 0x0d         /*     01_101 */
#define CHAR_TAG 0x25          /*    100_101 */
#define NIL_TAG 0x45           /*   1000_101 */
#define EOFOBJ_TAG 0x85        /*  10000_101 */
#define HT_TOMBSTONE_TAG 0x105 /* 100000_101 */
#define SYMBOL_TAG 0x06

#define TAG_MASK 0x7
#define VALUE_MASK 0xfffffffffffffff8
#define BOOL_TAG_MASK 0xf
#define VOID_TAG_MASK 0x1f
#define CHAR_TAG_MASK 0x3f
#define SYMBOL_TAG_MASK 0x7
#define EOFOBJ_TAG_MASK 0x1ff

#define FIXNUM(v) (value)((uint64_t)(v) << 3 | FIXNUM_TAG)
#define CLOSURE(v) (value)((uint64_t)(v) | CLOSURE_TAG)
#define PAIR(v) (value)((uint64_t)(v) | PAIR_TAG)
#define STRING(v) (value)((uint64_t)(v) | STRING_TAG)
#define BOOL(v) ((v) ? TRUE : FALSE)
#define CHAR(v) (value)((uint64_t)(v) << 32 | CHAR_TAG)
#define SYMBOL(v) (value)((uint64_t)(v) | SYMBOL_TAG)
#define OBJECT(v) (value)((uint64_t)(v) | OBJECT_TAG)

#define SENTINEL (value)(SENTINEL_TAG)
#define VOID (value)(VOID_TAG)
#define TRUE (value)(TRUE_TAG)
#define FALSE (value)(FALSE_TAG)
#define HT_TOMBSTONE (value)(HT_TOMBSTONE_TAG)
#define NIL (value)(NIL_TAG)
#define EOFOBJ (value)(EOFOBJ_TAG)

/************ hash table ***********/

#define HASH_TABLE_INITIAL_SIZE 8

struct hash_table;
typedef uint64_t (*hash_fn)(struct hash_table *ht, value key);
typedef int (*eq_fn)(struct hash_table *ht, value a, value b);

struct hash_table {
    hash_fn hash_fn;
    eq_fn eq_fn;
    value user_hash_fn;
    value user_eq_fn;
    size_t size;
    size_t cap;
    value *data;
};

/************ value types ***********/

struct closure {
    funcptr func;
    int min_args;
    int max_args;
    int n_freevars;
    value *freevars;
};

struct closure0 {
    funcptr func;
    int min_args;
    int max_args;
    int n_freevars;
    value *freevars;
};

struct closure1 {
    funcptr func;
    int min_args;
    int max_args;
    int n_freevars;
    value *freevars;
    value _freevars[1];
};

struct closure2 {
    funcptr func;
    int min_args;
    int max_args;
    int n_freevars;
    value *freevars;
    value _freevars[2];
};

struct closure3 {
    funcptr func;
    int min_args;
    int max_args;
    int n_freevars;
    value *freevars;
    value _freevars[3];
};

/* True if closure cl can accept exactly n arguments. */
#define CLOSURE_ACCEPTS(cl, n) ((cl)->min_args <= (n) && (n) <= (cl)->max_args)

struct pair {
    value car;
    value cdr;
};

struct string {
    size_t len;
    char *s;
};

enum sym_kind {
    sym_unbound, /* default (0 from calloc): no binding */
    sym_special, /* built-in syntax; value = canonical special form symbol */
    sym_value,   /* runtime value: primcall closure or user-defined variable */
    sym_macro,   /* macro transformer (future) */
};

struct symbol_name {
    size_t len;
    char name[];
};

struct symbol {
    size_t name_len;
    char *name;
    enum sym_kind kind;
    value value;
};

enum object_type {
    OBJ_PORT,
    OBJ_ERROR,
    OBJ_VECTOR,
    OBJ_WRAPPED,
    OBJ_BOX,
    OBJ_HASH_TABLE,
    OBJ_ENVIRONMENT,
};

enum port_direction {
    PORT_DIR_READ,
    PORT_DIR_WRITE,
};

enum error_type {
    ERR_OTHER,
    ERR_FILE, /* satisfies file-error? */
};

struct object {
    enum object_type type;
    union {
        struct {
            struct hash_table ht;
        } hash_table;
        struct {
            int direction;
            int closed;
            FILE *fp;
            char *filename;
            char *string;
            int64_t string_cap;
            int64_t string_len;
            value (*read_char)(value port);
            value (*peek_char)(value port);
            value (*read_line)(value port);
            void (*unread_char)(value port, value ch);
            void (*write_char)(value port, value ch);
            void (*printf)(value port, const char *fmt, ...);
        } port;
        struct {
            enum error_type type;
            int err_no;
        } error;
        struct {
            value *data;
            int64_t len;
        } vector;
        struct {
            value value;
            value kind;
        } wrapped;
        struct {
            value value;
        } box;
        struct {
            value hash_table; /* FALSE = global sentinel, hash table = local env */
        } environment;
    };
};

/************ accessors and predicates ***********/

#define GET_FIXNUM(v) ((int64_t)(v) >> 3)
#define GET_CLOSURE(v) ((struct closure *)((uint64_t)(v) & VALUE_MASK))
#define GET_BOOL(v) ((uint64_t)(v) >> 4)
#define GET_STRING(v) ((struct string *)((uint64_t)(v) & VALUE_MASK))
#define GET_PAIR(v) ((struct pair *)((uint64_t)(v) & VALUE_MASK))
#define GET_CHAR(v) ((char)((uint64_t)(v) >> 32))
#define GET_SYMBOL(v) ((struct symbol *)((uint64_t)(v) & VALUE_MASK))
#define GET_OBJECT(v) ((struct object *)((uint64_t)(v) & VALUE_MASK))

#define IS_FIXNUM(v) (((uint64_t)(v) & TAG_MASK) == FIXNUM_TAG)
#define IS_CLOSURE(v) (((uint64_t)(v) & TAG_MASK) == CLOSURE_TAG)
#define IS_STRING(v) (((uint64_t)(v) & TAG_MASK) == STRING_TAG)
#define IS_BOOL(v) (((uint64_t)(v) & BOOL_TAG_MASK) == BOOL_TAG)
#define IS_VOID(v) (((uint64_t)(v) & VOID_TAG_MASK) == VOID_TAG)
#define IS_CHAR(v) (((uint64_t)(v) & CHAR_TAG_MASK) == CHAR_TAG)
#define IS_SYMBOL(v) ((((uint64_t)(v) & SYMBOL_TAG_MASK) == SYMBOL_TAG))
#define IS_NIL(v) ((uint64_t)(v) == NIL_TAG)
#define IS_PAIR(v) (((uint64_t)(v) & TAG_MASK) == PAIR_TAG)
#define IS_EOFOBJ(v) ((uint64_t)(v) == EOFOBJ_TAG)
#define IS_OBJECT(v) (((uint64_t)(v) & TAG_MASK) == OBJECT_TAG)
#define IS_PORT(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_PORT)
#define IS_ERROR(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_ERROR)
#define IS_VECTOR(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_VECTOR)
#define IS_WRAPPED(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_WRAPPED)
#define IS_BOX(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_BOX)
#define IS_HASH_TABLE(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_HASH_TABLE)
#define IS_ENVIRONMENT(v) (IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_ENVIRONMENT)

/************ globals and helpers ***********/

#define RAISE(...) { print_stacktrace(); fprintf(stderr, "exception: " __VA_ARGS__); fprintf(stderr, "\n"); cleanup(); exit(1); }

#define init_args() va_list argsx; va_start(argsx, nargs); value *arg_arr_base = flags & CALL_HAS_ARG_ARRAY ? va_arg(argsx, value *) : NULL; value *arg_arr = arg_arr_base
#define reset_args() va_end(argsx); va_start(argsx, nargs); arg_arr = arg_arr_base
#define next_arg() (arg_arr == NULL ? va_arg(argsx, value) : *arg_arr++)
#define free_args() va_end(argsx); free(arg_arr_base)

/* primcall_apply malloc's an args array and passes it via
 * CALL_HAS_ARG_ARRAY. The callee's arg_arr_base is a raw malloc pointer
 * invisible to the conservative GC stack scan. This stack tracks active
 * arrays so the GC can scan their contents directly. */
struct args_array_frame {
    value *args;
    int    n;
    struct args_array_frame *prev;
};

struct kind_proc {
    value kind;
    value proc;
};

/************ memory management ***********/

#define POOL_SIZE 16384
#define ALIGN16(n) (((n) + 15) & ~15)

/* this is used as a header for all objects we allocate in a pool */
struct block {
    uint8_t in_use;
    uint8_t mark; /* for gc */
};

struct pool {
    struct pool *next;
    struct pool *prev;
    int in_use_count;
    int block_size; /* header plus actual object, aligned */
    int next_index; /* next index alloc_from_heap better start searching at */
    void *start;
    void *end;
    uint64_t tag; /* value tag expected for all objects in this pool */
};

/************ small/inline static functions ***********/

static void cleanup(void) {}

static value envget(environment env, int index) {
    value *vars = env;
    return vars[index];
}

/************ extern variable declarations ***********/

extern void *stack_start;

extern struct hash_table symbols;

extern int cmdline_argc;
extern const char **cmdline_argv;

/************ extern function declarations ***********/

extern void init_memory(void);
extern void init_ports(void);
extern value make_symbol(char *name, size_t len, enum sym_kind kind);
extern value make_closure(funcptr func, int min_args, int max_args, int nfreevars, ...);
extern value make_string(const char *s, size_t len);
extern value make_vector(size_t len, value fill);
extern value make_pair(value car, value cdr);
extern value reverse_list(value list, value acc);
extern void  print_stacktrace(void);
extern const char *find_func_name(funcptr func);
extern void env_define(value e, value sym, value val);
extern value env_ref(value e, value sym);

extern void hash_table_init(struct hash_table *ht, size_t initial_size, hash_fn hash_fn, eq_fn eq_fn);
extern void hash_table_set(struct hash_table *ht, volatile value owner, value k, value v);

extern void enter_proc(funcptr func);
extern void leave_proc(void);

extern uint64_t symbol_name_hash(struct hash_table *ht, value key);
extern int symbol_name_eq(struct hash_table *ht, value k1, value k2);

extern value primcall_apply(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_boolean_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_box(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_box_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_car(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_cdr(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_char_downcase(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_char_upcase(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_char_to_integer(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_char_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_close_port(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_cons(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_command_line(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_current_error_port(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_current_input_port(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_current_output_port(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_delete_file(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_display(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_eof_object_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_eq_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_error(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_error_object_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_exit(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_file_error_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_gensym(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_get_environment_variable(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_get_output_string(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_input_port_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_integer_to_char(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_make_string(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_make_vector(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_newline(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_number_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_number_to_string(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_open_input_file(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_open_output_file(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_open_output_string(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_pair_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_peek_char(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_port_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_procedure_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_read_char(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_read_line(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_set_box_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_set_car_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_set_cdr_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_to_number(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_to_symbol(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_symbol_to_string(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_append(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_copy(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_length(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_ref(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_set_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_eq_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_ci_eq_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_substring(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_symbol_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_system(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_unread_char(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_urandom(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_unbox(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_unwrap(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_vector_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_vector_length(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_vector_ref(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_vector_set_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_void(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_wrap(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_wrapped_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_wrapped_kind(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_wrapped_set_print(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_write(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_write_char(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_add(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_div(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_mul(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_sub(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_num_eq(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_num_lt(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_num_gt(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_num_le(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_num_ge(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_percent_make_hash_table(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_set_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_delete_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_ref(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_ref_default(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_exists_q(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_size(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_update_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_update_b_default(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_keys(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_values(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_walk(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_fold(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_to_alist(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_copy(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_merge_b(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_by_identity(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_hash(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_string_ci_hash(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_equivalence_function(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_hash_table_hash_function(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_make_environment(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_environment_ref(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_environment_define(environment env, enum call_flags flags, int nargs, ...);
extern value primcall_environment_q(environment env, enum call_flags flags, int nargs, ...);

/************ static library registration ***********/

#define STATIC_LIB_CONSTRUCTOR(fn) \
    __attribute__((constructor)) static void fn(void)

struct static_lib {
    value (*init)(value env);
    struct static_lib *next;
};

extern void register_static_lib(struct static_lib *lib);
extern void run_static_libs(value env);
