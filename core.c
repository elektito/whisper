#include "core.h"

#include <dlfcn.h>

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <setjmp.h>
#include <stdint.h>

/*************** static variables **************/

static uint64_t gensym_counter;

static int n_wrapped_print_procs = 0;
static struct kind_proc *wrapped_print_procs = NULL;

static struct object current_input_port;
static struct object current_output_port;
static struct object current_error_port;

/* both in units of allocations (object count), not bytes */
static size_t allocations_since_gc = 0;
static size_t gc_threshold = POOL_SIZE;

/* multiplier applied to the live count to pick the next gc_threshold.
 * overridable via GC_THRESHOLD_MULTIPLIER for experimentation. */
static int gc_threshold_multiplier = 0;

/* how much floating garbage we tolerate before promoting a gc mark to a
 * full mark-and-sweep. a multiplier of the number of floating garbage.
 * Can be overriden via GC_FULL_SWEEP_RATIO environment variable. */
static int gc_full_sweep_ratio = 0;

/* we're gonna call a linked list of pools, a heap. */
static struct pool *symbols_heap;
static struct pool *pairs_heap;
static struct pool *objects_heap;
static struct pool *strings_heap;
static struct pool *closure0s_heap;
static struct pool *closure1s_heap;
static struct pool *closure2s_heap;
static struct pool *closure3s_heap;
static struct pool *closures_heap;

static void *lbound_all_heaps = 0;
static void *ubound_all_heaps = 0;

static uint64_t gc_epoch = 0;
static int gc_manual_mode = 0;
static size_t gc_marked_count = 0;

static size_t gc_full_sweeps = 0;
static size_t gc_lazy_reclaims = 0;

static struct pool **heaps;
static int n_heaps = 0;

/*************** non-static variables **************/

/* the stack address at the start of main function (used for gc) */
void *stack_start;

/* interned symbols: maps struct symbol_name to symbol objects */
struct hash_table symbols;

int cmdline_argc;
const char **cmdline_argv;

/*************** stack trace **************/

/* these are also used by find_func_name so it's defined outside ifdef */
static void hash_table_each(struct hash_table *ht, void (*fn)(value k, value v, void *ctx), void *ctx);
struct symbol_ht_ctx {
    char *name;
    size_t name_len;
    funcptr func;
};

static void symbols_ht_each(value k, value v, void *ctx) {
    struct symbol_ht_ctx *c = ctx;
    struct symbol *sym = GET_SYMBOL(v);
    if (IS_CLOSURE(sym->value) && GET_CLOSURE(sym->value)->func == c->func) {
        c->name = GET_SYMBOL(v)->name;
        c->name_len = GET_SYMBOL(v)->name_len;
    }
}


#ifdef DEBUG

static funcptr *stacktrace = NULL;
static int stacktrace_size = 0;
static int stacktrace_cap = 0;

void enter_proc(funcptr func) {
    if (stacktrace_size == stacktrace_cap) {
        stacktrace_cap *= 2;
        if (stacktrace_cap == 0) {
            stacktrace_cap = 16;
        }

        stacktrace = realloc(stacktrace, stacktrace_cap * sizeof(funcptr));
    }

    stacktrace[stacktrace_size++] = func;
}

void leave_proc(void) {
    stacktrace_size--;
}

/* unlike most our functions, this one is not static so hopefully it
 * won't be inlined and be still availble in the debugger, in case we
 * want to call it directly. */
void print_stacktrace(void) {
    if (stacktrace_size == 0) {
        fprintf(stderr, "Stacktrace is empty.\n");
        return;
    }

    fprintf(stderr, "NOTE: unnamed lambdas will not show up in the stack trace.\n");

    int idx = 1;
    for (int i = 0; i < stacktrace_size; ++i) {
        struct symbol_ht_ctx ctx;
        ctx.name_len = 0;
        ctx.func = stacktrace[i];
        hash_table_each(&symbols, symbols_ht_each, &ctx);

        /* unknown names are either let blocks, or unnamed lambdas.
         * would have been nice if we could detect the difference and
         * for the lambda's show an entry in the stacktrace. */
        if (ctx.name_len == 0) {
            continue;
        }

        fprintf(stderr, "[%d] %.*s\n", idx++, (int)ctx.name_len, ctx.name);
    }
}

#else
void print_stacktrace(void) {}
#endif /* DEBUG */

const char *find_func_name(funcptr func) {
    char *buf;
    struct symbol_ht_ctx ctx;
    ctx.name_len = 0;
    ctx.func = func;
    hash_table_each(&symbols, symbols_ht_each, &ctx);
    if (ctx.name_len == 0) {
        const char *unknown = "(unknown)";
        buf = malloc(strlen(unknown) + 1);
        memcpy(buf, unknown, strlen(unknown));
        buf[strlen(unknown)] = 0;
    } else {
        buf = malloc(ctx.name_len + 1);
        memcpy(buf, ctx.name, ctx.name_len);
        buf[ctx.name_len] = 0;
    }

    return buf;
}

/************ hash table ***********/

/* A good hash function for direct use with tagged pointers, which are
 * basically 64-bit integers. Translated from java, from:
 * https://gist.github.com/badboy/6267743 */
static uint64_t hash_table_default_hash(struct hash_table *ht, value key)
{
    uint64_t k = (uint64_t) key;
    k = (~k) + (k << 21); /* key = (key << 21) - key - 1; */
    k = k ^ (k >> 24);
    k = (k + (k << 3)) + (k << 8); /* key * 265 */
    k = k ^ (k >> 14);
    k = (k + (k << 2)) + (k << 4); /* key * 21 */
    k = k ^ (k >> 28);
    k = k + (k << 31);
    return k;
}

static int hash_table_default_eq(struct hash_table *ht, value k1, value k2) {
    return k1 == k2;
}

void hash_table_init(struct hash_table *ht, size_t initial_size, hash_fn hash_fn, eq_fn eq_fn) {
    ht->data = malloc(2 * initial_size * sizeof(value)); /* double because key+value */
    ht->size = 0;
    ht->cap = initial_size;

    ht->hash_fn = hash_fn ? hash_fn : hash_table_default_hash;
    ht->eq_fn = eq_fn ? eq_fn : hash_table_default_eq;
    ht->user_hash_fn = FALSE;
    ht->user_eq_fn = FALSE;

    /* initialize keys and values to sentinel values to mark them as
     * empty */
    for (int i = 0; i < initial_size * 2; ++i) {
        ht->data[i] = SENTINEL;
    }
}

static void hash_table_cleanup(struct hash_table *ht) {
    free(ht->data);
}

/* owner is the tagged value of the object containing ht. It is kept
 * volatile to force a stack spill, ensuring the conservative GC finds
 * the reference if a user-provided hash or eq function triggers collection. */
static void hash_table_grow(struct hash_table *ht, volatile value owner) {
    value *old_data = ht->data;
    size_t old_cap = ht->cap;
    ht->cap *= 2;
    ht->data = malloc(2 * ht->cap * sizeof(value)); /* double because key+value */

    /* initialize keys and values to sentinel values to mark them as
     * empty */
    for (int i = 0; i < ht->cap * 2; ++i) {
        ht->data[i] = SENTINEL;
    }

    for (int old_idx = 0; old_idx < old_cap; ++old_idx) {
        value key = old_data[old_idx * 2];
        if (key != SENTINEL && key != HT_TOMBSTONE) {
            uint64_t h = ht->hash_fn(ht, key);
            size_t new_idx = h % ht->cap;
            while (ht->data[new_idx * 2] != SENTINEL) {
                new_idx = (new_idx + 1) % ht->cap;
            }
            ht->data[new_idx * 2] = old_data[old_idx * 2];
            ht->data[new_idx * 2 + 1] = old_data[old_idx * 2 + 1];
        }
    }

    free(old_data);
}

/* owner: see hash_table_grow */
void hash_table_set(struct hash_table *ht, volatile value owner, value k, value v) {
    if (ht->size * 10 >= ht->cap * 7) { /* 70% */
        hash_table_grow(ht, owner);
    }

    uint64_t h = ht->hash_fn(ht, k);
    size_t start = h % ht->cap;
    size_t idx = start;
    size_t tombstone_idx = SIZE_MAX;

    do {
        if (ht->data[idx * 2] == SENTINEL) {
            size_t i = tombstone_idx == SIZE_MAX ? idx : tombstone_idx;
            ht->data[i * 2] = k;
            ht->data[i * 2 + 1] = v;
            ht->size++;
            return;
        } else if (ht->data[idx * 2] == HT_TOMBSTONE) {
            if (tombstone_idx == SIZE_MAX) {
                tombstone_idx = idx;
            }
        } else if (ht->eq_fn(ht, ht->data[idx * 2], k)) {
            ht->data[idx * 2 + 1] = v;
            return;
        }

        idx = (idx + 1) % ht->cap;
    } while (idx != start);

    assert (tombstone_idx != SIZE_MAX);
    ht->data[tombstone_idx * 2] = k;
    ht->data[tombstone_idx * 2 + 1] = v;
    ht->size++;
}

/* owner: see hash_table_grow */
static int hash_table_delete(struct hash_table *ht, volatile value owner, value key) {
    uint64_t h = ht->hash_fn(ht, key);
    size_t start = h % ht->cap;
    size_t idx = start;

    do {
        if (ht->data[idx * 2] == SENTINEL) {
            /* not found */
            return 0;
        } else if (ht->data[idx * 2] != HT_TOMBSTONE && ht->eq_fn(ht, ht->data[idx * 2], key)) {
            ht->data[idx * 2] = HT_TOMBSTONE;
            ht->size--;
            return 1;
        }

        idx = (idx + 1) % ht->cap;
    } while (idx != start);

    /* not found */
    return 0;
}

/* owner: see hash_table_grow */
static value hash_table_get(struct hash_table *ht, volatile value owner, value key) {
    uint64_t h = ht->hash_fn(ht, key);

    size_t start = h % ht->cap;
    size_t idx = start;

    do {
        if (ht->data[idx * 2] == SENTINEL) {
            /* not found */
            return SENTINEL;
        } else if (ht->data[idx * 2] != HT_TOMBSTONE && ht->eq_fn(ht, ht->data[idx * 2], key)) {
            return ht->data[idx * 2 + 1];
        }

        idx = (idx + 1) % ht->cap;
    } while (idx != start);

    /* not found */
    return SENTINEL;
}

static void hash_table_each(struct hash_table *ht, void (*fn)(value k, value v, void *ctx), void *ctx) {
    for (int i = 0; i < ht->cap; ++i) {
        value key = ht->data[i * 2];
        if (key != SENTINEL && key != HT_TOMBSTONE) {
            fn(key, ht->data[i * 2 + 1], ctx);
        }
    }
}

static uint64_t hash_string_djb2(const char *s, size_t len);

/* hash function for struct symbol_name */
uint64_t symbol_name_hash(struct hash_table *ht, value key) {
    struct symbol_name *sn = (struct symbol_name *) key;
    return hash_string_djb2(sn->name, sn->len);
}

/* equivalence function for struct symbol_name */
int symbol_name_eq(struct hash_table *ht, value k1, value k2) {
    struct symbol_name *sn1 = (struct symbol_name *) k1;
    struct symbol_name *sn2 = (struct symbol_name *) k2;
    if (sn1->len != sn2->len)
        return 0;
    return memcmp(sn1->name, sn2->name, sn1->len) == 0;
}


/************ gc/memory helper functions ***********/

static struct pool *create_heap(int object_size, uint64_t tag) {
    int block_size = ALIGN8(sizeof(struct block)) + ALIGN8(object_size);

    int total_size = sizeof(struct pool) + block_size * POOL_SIZE;
    struct pool *pool = calloc(1, total_size);
    pool->prev = NULL;
    pool->next = NULL;
    pool->in_use_count = 0;
    pool->block_size = block_size;
    pool->start = pool + 1; /* one struct pool ahead */
    pool->end = (void*) pool + total_size;
    pool->tag = tag;

    if (lbound_all_heaps == 0 || lbound_all_heaps > pool->start) {
        lbound_all_heaps = pool->start;
    }
    if (ubound_all_heaps == 0 || ubound_all_heaps < pool-> end) {
        ubound_all_heaps = pool->end;
    }

    pool->alloc_cursor = pool;

    return pool;
}

static struct pool *add_pool(struct pool *pool) {
    while (pool->next) pool = pool->next;

    int header_size = ALIGN8(sizeof(struct pool));
    int total_size = header_size + pool->block_size * POOL_SIZE;
    struct pool *new_pool = calloc(1, total_size);
    pool->next = new_pool;
    new_pool->prev = pool;
    new_pool->next = NULL;
    new_pool->in_use_count = 0;
    new_pool->block_size = pool->block_size;
    new_pool->next_index = 0;
    new_pool->start = (void*) new_pool + header_size;
    new_pool->end = (void*) new_pool + total_size;
    new_pool->tag = pool->tag;

    if (lbound_all_heaps == 0 || lbound_all_heaps > new_pool->start) {
        lbound_all_heaps = new_pool->start;
    }
    if (ubound_all_heaps == 0 || ubound_all_heaps < new_pool-> end) {
        ubound_all_heaps = new_pool->end;
    }

    return new_pool;
}

void init_memory(void) {
    symbols_heap = create_heap(sizeof(struct symbol), SYMBOL_TAG);
    pairs_heap = create_heap(sizeof(struct pair), PAIR_TAG);
    objects_heap = create_heap(sizeof(struct object), OBJECT_TAG);
    strings_heap = create_heap(sizeof(struct string), STRING_TAG);
    closure0s_heap = create_heap(sizeof(struct closure0), CLOSURE_TAG);
    closure1s_heap = create_heap(sizeof(struct closure1), CLOSURE_TAG);
    closure2s_heap = create_heap(sizeof(struct closure2), CLOSURE_TAG);
    closure3s_heap = create_heap(sizeof(struct closure3), CLOSURE_TAG);
    closures_heap = create_heap(sizeof(struct closure), CLOSURE_TAG);

    n_heaps = 9;
    heaps = malloc(n_heaps * sizeof(struct pool *));
    heaps[0] = symbols_heap;
    heaps[1] = pairs_heap;
    heaps[2] = objects_heap;
    heaps[3] = strings_heap;
    heaps[4] = closure0s_heap;
    heaps[5] = closure1s_heap;
    heaps[6] = closure2s_heap;
    heaps[7] = closure3s_heap;
    heaps[8] = closures_heap;
}

static void gc_recurse(value v);
static void gc_recurse_env_ht_each(value k, value v, void *ctx) {
    struct binding *binding = (struct binding *) v;
    gc_recurse(k);
    gc_recurse(binding->value);
}

static void gc_recurse(value v) {
    if (!IS_SYMBOL(v) && !IS_PAIR(v) && !IS_OBJECT(v) && !IS_STRING(v) && !IS_CLOSURE(v)) {
        return;
    }

    struct block *block = (struct block*)(((uint64_t) v & VALUE_MASK) - ALIGN8(sizeof(struct block)));
    if (!block->in_use || block->gc_epoch == gc_epoch) {
        return;
    }

    if (IS_SYMBOL(v)) {
        gc_marked_count++;
        block->gc_epoch = gc_epoch;
        if (GET_SYMBOL(v)->kind == sym_value) {
            gc_recurse(GET_SYMBOL(v)->value);
        }
    } else if (IS_PAIR(v)) {
        /* iterate along the cdr chain instead of recursing, to avoid
         * O(n) stack depth for lists of length n. we still recurse on
         * car, but that is bounded by tree depth rather than list
         * length. */
        while (IS_PAIR(v)) {
            gc_marked_count++;
            block->gc_epoch = gc_epoch;
            gc_recurse(GET_PAIR(v)->car);
            v = GET_PAIR(v)->cdr;
            if (!IS_PAIR(v) && !IS_OBJECT(v) && !IS_STRING(v) && !IS_CLOSURE(v))
                return;
            block = (struct block*)(((uint64_t) v & VALUE_MASK) - ALIGN8(sizeof(struct block)));
            if (!block->in_use || block->gc_epoch == gc_epoch)
                return;
        }

        /* handle the tail of an improper list (e.g. (a b . c)) */
        gc_recurse(v);
    } else if (IS_OBJECT(v)) {
        gc_marked_count++;
        block->gc_epoch = gc_epoch;
        if (GET_OBJECT(v)->type == OBJ_VECTOR) {
            for (int i = 0; i < GET_OBJECT(v)->vector.len; ++i) {
                gc_recurse(GET_OBJECT(v)->vector.data[i]);
            }
        } else if (GET_OBJECT(v)->type == OBJ_WRAPPED) {
            gc_recurse(GET_OBJECT(v)->wrapped.value);
            gc_recurse(GET_OBJECT(v)->wrapped.kind);
        } else if (GET_OBJECT(v)->type == OBJ_BOX) {
            gc_recurse(GET_OBJECT(v)->box.value);
        } else if (GET_OBJECT(v)->type == OBJ_HASH_TABLE) {
            struct hash_table *ht = &GET_OBJECT(v)->hash_table.ht;
            gc_recurse(ht->user_hash_fn);
            gc_recurse(ht->user_eq_fn);
            for (int i = 0; i < ht->cap; ++i) {
                if (ht->data[i * 2] != SENTINEL && ht->data[i * 2] != HT_TOMBSTONE) {
                    gc_recurse(ht->data[i * 2]);
                    gc_recurse(ht->data[i * 2 + 1]);
                }
            }
        } else if (GET_OBJECT(v)->type == OBJ_ENVIRONMENT) {
            if (GET_OBJECT(v)->environment.hash_table != NULL)
                hash_table_each(GET_OBJECT(v)->environment.hash_table, gc_recurse_env_ht_each, NULL);
        }
    } else if (IS_STRING(v)) {
        gc_marked_count++;
        block->gc_epoch = gc_epoch;
    } else if (IS_CLOSURE(v)) {
        gc_marked_count++;
        block->gc_epoch = gc_epoch;
        for (int i = 0; i < GET_CLOSURE(v)->n_freevars; ++i) {
            gc_recurse(GET_CLOSURE(v)->freevars[i]);
        }
    }
}

static int is_valid_value(void *ptr, struct pool *heap) {
    /* the pointer should have a valid address and a correct tag to be a
     * valid value */

    if (ptr < lbound_all_heaps || ptr >= ubound_all_heaps)
        return 0;

    struct pool *pool = heap;
    int found = 0;
    while (pool) {
        if (ptr >= pool->start && ptr < pool->end) {
            void *block_start = (void*)((uint64_t)ptr & VALUE_MASK) - ALIGN8(sizeof(struct block));
            if ((block_start - pool->start) % pool->block_size == 0) {
                found = 1;
                break;
            }
        }

        pool = pool->next;
    }

    if (!found)
        return 0;

    return ((uint64_t)ptr & TAG_MASK) == pool->tag;
}

struct freevars_closure_mapping {
    uint64_t freevars;
    value    closure;
};

/* Build a map from each large closure's malloc'd freevars pointer to its
 * closure value.  The caller must free() the returned array. */
static struct freevars_closure_mapping *build_freevars_map(int *out_len) {
    /* calculate the number of in-use (large) closure objects */
    int len = 0;
    for (struct pool *pool = closures_heap; pool; pool = pool->next)
        for (void *b = pool->start; b < pool->end; b += pool->block_size)
            if (((struct block *)b)->in_use)
                len++;

    /* create a map from malloc'd freevars pointers to their associated
     * closure objects */
    struct freevars_closure_mapping *map = NULL;
    if (len > 0) {
        map = malloc(len * sizeof(*map));
        int idx = 0;
        for (struct pool *pool = closures_heap; pool; pool = pool->next) {
            for (void *b = pool->start; b < pool->end; b += pool->block_size) {
                struct block *blk = b;
                if (blk->in_use) {
                    struct closure *cl = b + ALIGN8(sizeof(struct block));
                    map[idx].freevars = (uint64_t)cl->freevars;
                    map[idx].closure  = CLOSURE(cl);
                    idx++;
                }
            }
        }
    }

    *out_len = len;
    return map;
}

/* see gc_mark() function's comment to see why no_sanitize */
__attribute__((no_sanitize("address")))
static void gc_scan_stack(void *cur_stack, struct pool **heaps, int n_heaps) {
    int freevars_map_len;
    struct freevars_closure_mapping *freevars_map = build_freevars_map(&freevars_map_len);

    for (void **p = cur_stack; p < (void**) stack_start; p++) {
        uint64_t tag = (uint64_t) *p & TAG_MASK;
        for (int i = 0; i < n_heaps; ++i) {
            if (heaps[i]->tag != tag) {
                continue;
            }
            if (is_valid_value(*p, heaps[i])) {
                gc_recurse(*p);
                break;
            }
        }

        /* Our code strips tags when accessing heap objects (e.g.
         * GET_PAIR does ptr & ~7), leaving an untagged pointer. The
         * compiler may keep this untagged pointer and discard the
         * original tagged value once it is no longer needed. If GC runs
         * while only the untagged pointer is live, the object appears
         * unreachable.
         *
         * Untagged pointers have zero tag bits, just like fixnums, but
         * heap addresses are always far above usual fixnum values so
         * false positives are unlikely (but possible, which is what we
         * accept in a conservative garbage collector).
         *
         * For large closures env points to a malloc'd array outside any
         * pool; those are matched via freevars_map. */
        uint64_t raw = (uint64_t)*p;
        if ((raw & TAG_MASK) == 0 && raw > 0) {
            for (int i = 0; i < n_heaps; ++i) {
                struct pool *pool = heaps[i];
                while (pool) {
                    if ((void *)raw >= pool->start && (void *)raw < pool->end) {
                        size_t offset = (uint8_t *)raw - (uint8_t *)pool->start;
                        void *block_start = (uint8_t *)pool->start
                                            + (offset / pool->block_size) * pool->block_size;
                        struct block *blk = (struct block *)block_start;
                        if (blk->in_use) {
                            void *obj = block_start + ALIGN8(sizeof(struct block));
                            gc_recurse((value)((uint64_t)obj | pool->tag));
                        }
                        break;
                    }
                    pool = pool->next;
                }
            }

            for (int j = 0; j < freevars_map_len; j++) {
                if (freevars_map[j].freevars == raw) {
                    gc_recurse(freevars_map[j].closure);
                    break;
                }
            }
        }
    }

    free(freevars_map);
}

static void gc_free_empty_pools(struct pool **heaps, int n_heaps) {
    for (int i = 0; i < n_heaps; ++i) {
        struct pool *pool = heaps[i]->next;  /* skip head — owned by global */
        while (pool) {
            struct pool *next = pool->next;
            if (pool->in_use_count == 0) {
                pool->prev->next = next;
                if (next)
                    next->prev = pool->prev;

                /* we could update lbound_all_heaps and ubound_all_heaps
                 * here too, but those are for quick checks anyways and
                 * updating them here would be a bit messy. */

                free(pool);
            }

            pool = next;
        }
    }
}

static void gc_sweep_env_ht_each(value k, value v, void *ctx) {
    struct binding *binding = (struct binding *) v;
    free(binding);
}

static void gc_free_block(void *p, struct pool *heap) {
    void *v = p + ALIGN8(sizeof(struct block));
    if (heap == symbols_heap) {
        free(((struct symbol *) v)->name);
    } else if (heap == strings_heap) {
        free(((struct string *) v)->s);
    } else if (heap == closures_heap) {
        free(((struct closure *) v)->freevars);
    } else if (heap == objects_heap) {
        struct object *obj = (struct object *) v;
        switch (obj->type) {
        case OBJ_PORT:
            free(obj->port.filename);
            free(obj->port.string);
            break;
        case OBJ_VECTOR:
            free(obj->vector.data);
            break;
        case OBJ_ENVIRONMENT:
            if (obj->environment.hash_table != NULL) {
                hash_table_each(obj->environment.hash_table, gc_sweep_env_ht_each, NULL);
                hash_table_cleanup(obj->environment.hash_table);
                free(obj->environment.hash_table);
            }
            break;
        default:
            /* do nothing */
            break;
        }
    }

    /* zero the data so stale pointers don't cause double-frees if the
     * block is reallocated before being fully initialized and GC runs
     * again */
    memset(v, 0, heap->block_size - ALIGN8(sizeof(struct block)));
}

static void gc_sweep(struct pool **heaps, int n_heaps) {
    /* free unmarked objects and reset marks */
    for (int i = 0; i < n_heaps; ++i) {
        struct pool *pool = heaps[i];
        while (pool) {
            for (void *p = pool->start; p < pool->end; p += pool->block_size) {
                struct block *block = p;

                if (block->in_use && block->gc_epoch != gc_epoch) {
                    gc_free_block(p, heaps[i]);

                    block->in_use = 0;
                    pool->in_use_count--;
                }
            }

            pool = pool->next;
        }
    }

}

static void gc_symbol_each(value k, value v, void *ctx) {
    gc_recurse(v);
}

/* this function would cause a false positive stack-buffer-overflow with
 * address sanitizer. address sanitizer itself says this about the issue:
 *
 * HINT: this may be a false positive if your program uses some custom
      stack unwind mechanism, swapcontext or vfork (longjmp and C++
      exceptions *are* supported)

 * since we are indeed doing some sort of custom stack unwinding, this
 * seems to fall exactly into this category. */
__attribute__((no_sanitize("address")))
static void gc_mark(void) {
    /* Spill callee-saved registers (rbx, r12-r15 on x86_64) onto the
     * stack before we scan it. The C compiler is free to keep live
     * Scheme values in those registers across calls; without this they
     * would be invisible to the stack scan and their referents would be
     * incorrectly collected. setjmp() is defined to save all
     * callee-saved registers into env, which lives on this stack frame
     * and is therefore covered by the scan below. */
    jmp_buf env;
    (void) setjmp(env);
    void *cur_stack = &env;

    gc_marked_count = 0;
    gc_epoch++;

    /* recursively mark values accessible from global symbols */
    hash_table_each(&symbols, gc_symbol_each, NULL);

    gc_scan_stack(cur_stack, heaps, n_heaps);

    if (gc_threshold_multiplier == 0) {
        char *env = getenv("GC_THRESHOLD_MULTIPLIER");
        gc_threshold_multiplier = env ? atoi(env) : 0;
        if (gc_threshold_multiplier <= 0)
            gc_threshold_multiplier = 4;
    }

    /* set next "full" (including sweep) gc threshold. floor at
     * POOL_SIZE so we don't GC on every allocation when the live set is
     * very small */
    size_t scaled = gc_marked_count * gc_threshold_multiplier;
    gc_threshold = scaled < POOL_SIZE ? POOL_SIZE : scaled;
    allocations_since_gc = 0;

    /* reset alloc_cursor for all heaps to the head */
    for (int i = 0; i < n_heaps; ++i) {
        heaps[i]->alloc_cursor = heaps[i];
    }
}

static void gc(void) {
    gc_mark();
    gc_sweep(heaps, n_heaps);
    gc_free_empty_pools(heaps, n_heaps);

    ++gc_full_sweeps;
}

static void gc_auto(void) {
    gc_mark();

    size_t in_use_total = 0;
    for (int i = 0; i < n_heaps; ++i) {
        for (struct pool *p = heaps[i]; p; p = p->next) {
            in_use_total += p->in_use_count;
        }
    }

    /* dead, but not yet reclaimed */
    size_t floating = in_use_total - gc_marked_count;

    if (gc_full_sweep_ratio == 0) {
        char *env = getenv("GC_FULL_SWEEP_RATIO");
        gc_full_sweep_ratio = env ? atoi(env) : 0;
        if (gc_full_sweep_ratio <= 0) {
            gc_full_sweep_ratio = 1;
        }
    }

    if (floating > gc_full_sweep_ratio * gc_marked_count) {
        gc_sweep(heaps, n_heaps);
        gc_free_empty_pools(heaps, n_heaps);
        ++gc_full_sweeps;
    }
}

static void *alloc_from_heap(struct pool *head) {
    if (allocations_since_gc >= gc_threshold && !gc_manual_mode) {
        gc_auto();
    }

    struct pool *start_cursor = head->alloc_cursor;
    struct pool *pool = start_cursor;
    for (;;) { /* in case the pool is actually full but the flag not set */
        for (int i = 0; i < POOL_SIZE; ++i) {
            int j = (i + pool->next_index) % POOL_SIZE;
            struct block *block = pool->start + pool->block_size * j;

            /* if unused, or not-yet-reclaimed */
            int reclaim = !gc_manual_mode && \
                block->in_use && \
                block->gc_epoch != gc_epoch;
            if (!block->in_use || reclaim) {
                if (reclaim) {
                    gc_free_block(block, head);
                    ++gc_lazy_reclaims;
                } else {
                    pool->in_use_count++;
                }

                block->in_use = 1;
                allocations_since_gc++;

                pool->next_index = (j + 1) % POOL_SIZE;

                /* mark this block as live now */
                block->gc_epoch = gc_epoch;

                /* advance alloc_cursor to current pool so we start
                 * scanning from this pool next time */
                head->alloc_cursor = pool;

                /* actual object starts after the block header (i.e. one
                 * struct block ahead) */
                return ((void*) block) + ALIGN8(sizeof(struct block));
            }
        }

        pool = pool->next;
        if (!pool) {
            pool = head;
        }

        if (pool == start_cursor) {
            pool = add_pool(pool);
        }
    }
}

static struct symbol *alloc_symbol(void) {
    return alloc_from_heap(symbols_heap);
}

static struct pair *alloc_pair(void) {
    return alloc_from_heap(pairs_heap);
}

static struct object *alloc_object(void) {
    return alloc_from_heap(objects_heap);
}

static struct string *alloc_string(size_t len, char fill) {
    struct string *str = alloc_from_heap(strings_heap);
    str->len = len;
    str->s = malloc(len);
    memset(str->s, fill, len);
    return str;
}

static struct closure *alloc_closure(int nfreevars) {
    struct closure *closure;
    struct closure0 *closure0;
    struct closure1 *closure1;
    struct closure2 *closure2;
    struct closure3 *closure3;

    switch (nfreevars) {
    case 0:
        closure0 = alloc_from_heap(closure0s_heap);
        closure0->freevars = NULL;
        return (struct closure *) closure0;
    case 1:
        closure1 = alloc_from_heap(closure1s_heap);
        closure1->freevars = closure1->_freevars;
        return (struct closure *) closure1;
    case 2:
        closure2 = alloc_from_heap(closure2s_heap);
        closure2->freevars = closure2->_freevars;
        return (struct closure *) closure2;
    case 3:
        closure3 = alloc_from_heap(closure3s_heap);
        closure3->freevars = closure3->_freevars;
        return (struct closure *) closure3;
    default:
        closure = alloc_from_heap(closures_heap);
        closure->freevars = calloc(1, nfreevars * sizeof(value));
        return closure;
    }
}

/************ pair/vector/string/symbol functions ***********/

value make_symbol(char *name, size_t len, enum sym_kind kind) {
    struct symbol *sym = alloc_symbol();
    sym->name = name;
    sym->name_len = len;
    sym->kind = kind;
    sym->value = VOID;
    return SYMBOL(sym);
}

value make_closure(funcptr func, int min_args, int max_args, int nfreevars, ...) {
    va_list args;
    struct closure *closure = alloc_closure(nfreevars);
    closure->func = func;
    closure->min_args = min_args;
    closure->max_args = max_args;
    closure->n_freevars = nfreevars;
    va_start(args, nfreevars);
    for (int i = 0; i < nfreevars; ++i) {
        closure->freevars[i] = va_arg(args, value);
    };
    va_end(args);
    return CLOSURE(closure);
}

value make_pair(value car, value cdr) {
    struct pair *pair = alloc_pair();
    pair->car = car;
    pair->cdr = cdr;
    return PAIR(pair);
}

value reverse_list(value list, value acc) {
    /* Iterative: read car/cdr before make_pair so GC cannot free the pair
     * between field reads (C argument evaluation order is unspecified). */
    while (list != NIL) {
        struct pair *p = GET_PAIR(list);
        value car = p->car;
        value cdr = p->cdr;
        acc  = make_pair(car, acc);
        list = cdr;
    }
    return acc;
}

value make_string(const char *s, size_t len) {
    struct string *p = alloc_string(len, '\0');
    memcpy(p->s, s, len);
    return STRING(p);
}

value make_vector(size_t len, value fill) {
    struct object *obj = alloc_object();
    obj->type = OBJ_VECTOR;
    obj->vector.len = len;
    obj->vector.data = calloc(obj->vector.len, sizeof(value));
    for (int i = 0; i < len; ++i) {
        obj->vector.data[i] = fill;
    }
    return OBJECT(obj);
}


/************ display/write helper functions ***********/

static char *strz(value str) {
    char *buf = malloc(GET_STRING(str)->len + 1);
    memcpy(buf, GET_STRING(str)->s, GET_STRING(str)->len);
    buf[GET_STRING(str)->len] = 0;
    return buf;
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

static void file_unread_char(value port, value ch) {
    FILE *fp = GET_OBJECT(port)->port.fp;
    int ret = ungetc(GET_CHAR(ch), fp);
    if (ret == EOF) { RAISE("error unreading character"); }
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
    int64_t needed = GET_OBJECT(port)->port.string_len + 1;
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

    /* we add 1 to the total needed, because vsnprintf writes a null
     * character at the end and if we're right at the end of the buffer
     * and there's only one character remaining, it will only write a
     * null character. this way, we make sure the null char is always
     * after actual data. */
    int64_t total_needed = op->port.string_len + extra_needed + 1;
    if (total_needed > op->port.string_cap) {
        while (op->port.string_cap < total_needed) op->port.string_cap *= 2;
        op->port.string = realloc(op->port.string, op->port.string_cap);
    }

    va_start(args, fmt);
    vsnprintf(op->port.string + op->port.string_len, op->port.string_cap - op->port.string_len, fmt, args);
    op->port.string_len += extra_needed;
    va_end(args);
}

static void _write(value v, value port);
static void print_unprintable(value v, value port) {
    if (IS_CLOSURE(v)) {
        if (GET_CLOSURE(v)->min_args == GET_CLOSURE(v)->max_args)
            GET_OBJECT(port)->port.printf(port, "#<procedure nargs=%d>", GET_CLOSURE(v)->min_args);
        else if (GET_CLOSURE(v)->max_args == MAX_ARGS)
            GET_OBJECT(port)->port.printf(port, "#<procedure min_args=%d>", GET_CLOSURE(v)->min_args);
        else
            GET_OBJECT(port)->port.printf(port, "#<procedure min_args=%d max_args=%d>", GET_CLOSURE(v)->min_args, GET_CLOSURE(v)->max_args);
    } else if (IS_EOFOBJ(v)) {
        GET_OBJECT(port)->port.printf(port, "#<eof-object>");
    } else if (IS_PORT(v)) {
        struct object *op = GET_OBJECT(v);
        const char *dir = op->port.direction == PORT_DIR_READ ? "input" : "output";
        const char *kind = op->port.string ? "string-" : "";
        if (op->port.filename) {
            GET_OBJECT(port)->port.printf(port, "#<%s-%sport \"%s\">", dir, kind, op->port.filename);
        } else {
            GET_OBJECT(port)->port.printf(port, "#<%s-%sport>", dir, kind);
        }
    } else if (IS_ERROR(v)) {
        const char *kind = GET_OBJECT(v)->error.type == ERR_FILE ? "file-" : "";
        GET_OBJECT(port)->port.printf(port, "#<%serror>", kind);
    } else if (IS_WRAPPED(v)) {
        int found = 0;
        value kind = GET_OBJECT(v)->wrapped.kind;
        for (int i = 0; i < n_wrapped_print_procs; ++i) {
            if (wrapped_print_procs[i].kind == kind) {
                found = 1;
                value proc = wrapped_print_procs[i].proc;
                GET_CLOSURE(proc)->func(GET_CLOSURE(proc)->freevars, NO_CALL_FLAGS, 2, v, port);
            }
        }

        if (!found) {
            GET_OBJECT(port)->port.printf(port, "#<wrapped kind=");
            _write(GET_OBJECT(v)->wrapped.kind, port);
            GET_OBJECT(port)->port.printf(port, " value=");
            _write(GET_OBJECT(v)->wrapped.value, port);
            GET_OBJECT(port)->port.printf(port, ">");
        }
    } else if (IS_BOX(v)) {
        GET_OBJECT(port)->port.printf(port, "#<box value=");
        _write(GET_OBJECT(v)->box.value, port);
        GET_OBJECT(port)->port.printf(port, ">");
    } else if (IS_HASH_TABLE(v)) {
        GET_OBJECT(port)->port.printf(
            port, "#<hash-table size=%zu cap=%zu>",
            GET_OBJECT(v)->hash_table.ht.size,
            GET_OBJECT(v)->hash_table.ht.cap);
    } else {
        GET_OBJECT(port)->port.printf(port, "#<object-%p>", v);
    }
}

static void print_symbol(value sym, value port) {
    if (IS_OBJECT(sym)) {
        GET_OBJECT(port)->port.printf(port, "#:"); /* uninterned symbol prefix */
    }

    GET_OBJECT(port)->port.printf(port, "%.*s", (int) GET_SYMBOL(sym)->name_len, GET_SYMBOL(sym)->name);
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

/* vec is an untagged pointer. Safe only because _display never
   allocates. If that changes, the conservative GC could miss the vector
   and collect it. */
static void _display_vector(struct object *vec, value port) {
    GET_OBJECT(port)->port.printf(port, "#(");
    for (int i = 0; i < GET_OBJECT(vec)->vector.len; ++i) {
        _display(GET_OBJECT(vec)->vector.data[i], port);
        if (i != GET_OBJECT(vec)->vector.len - 1) {
            GET_OBJECT(port)->port.printf(port, " ");
        }
    }
    GET_OBJECT(port)->port.printf(port, ")");
}

static void _display(value v, value port) {
    if (IS_FIXNUM(v)) {
        GET_OBJECT(port)->port.printf(port, "%ld", GET_FIXNUM(v));
    } else if (IS_STRING(v)) {
        GET_OBJECT(port)->port.printf(port, "%.*s", (int) GET_STRING(v)->len, GET_STRING(v)->s);
    } else if (IS_SYMBOL(v)) {
        print_symbol(v, port);
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
    } else if (IS_VECTOR(v)) {
        _display_vector(GET_OBJECT(v), port);
    } else {
        print_unprintable(v, port);
    }
}

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

/* vec is an untagged pointer. Safe only because _write never allocates.
   If that changes, the conservative GC could miss the vector and
   collect it. */
static void _write_vector(struct object *vec, value port) {
    GET_OBJECT(port)->port.printf(port, "#(");
    for (int i = 0; i < GET_OBJECT(vec)->vector.len; ++i) {
        _write(GET_OBJECT(vec)->vector.data[i], port);
        if (i != GET_OBJECT(vec)->vector.len - 1) {
            GET_OBJECT(port)->port.printf(port, " ");
        }
    }
    GET_OBJECT(port)->port.printf(port, ")");
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
            sprintf(buf, "#\\x%02x", (int)(uint8_t) ch);
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
        print_symbol(v, port);
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
    } else if (IS_VECTOR(v)) {
        _write_vector(GET_OBJECT(v), port);
    } else {
        print_unprintable(v, port);
    }
}

/************ symbol helper functions ***********/

static value string_to_symbol(value v) {
    struct string *s = GET_STRING(v);
    struct symbol_name *name = malloc(sizeof(struct symbol_name) + s->len);
    name->len = s->len;
    memcpy(name->name, s->s, s->len);
    value sym = hash_table_get(&symbols, 0, name);
    if (sym != SENTINEL) {
        free(name);
        return sym;
    }

    char *buf = malloc(s->len);
    memcpy(buf, s->s, s->len);
    sym = make_symbol(buf, s->len, sym_unbound);
    hash_table_set(&symbols, 0, (value) name, sym);

    /* do not free name; it stays as the key to the hash table. */

    return sym;
}

static value symbol_to_string(value v) {
    struct symbol *sym = GET_SYMBOL(v);
    return make_string(sym->name, sym->name_len);
}

/************ list/pair helper functions ***********/

static int is_proper_list(value v) {
    value cur = v;
    for (;;) {
        if (cur == NIL) { return 1; }
        if (IS_PAIR(cur)) {
            cur = GET_PAIR(cur)->cdr;
        } else {
            return 0;
        }
    }
}

/************ string helper functions ***********/

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

static int string_ci_cmp(struct string *s1, struct string *s2) {
    if (s1->len != s2->len) {
        return s1->len < s2->len ? -1 : 1;
    }

    for (size_t i = 0; i < s1->len; i++) {
        int d = tolower((unsigned char) s1->s[i]) - tolower((unsigned char) s2->s[i]);
        if (d != 0) {
            return d;
        }
    }
    return 0;
}

/************ environment functions ***********/

void env_define(value e, value sym, value val, enum sym_kind kind) {
    struct hash_table *ht = GET_OBJECT(e)->environment.hash_table;
    if (ht == NULL) {
        GET_SYMBOL(sym)->value = val;
        GET_SYMBOL(sym)->kind = kind;
        return;
    }
    struct binding *binding = (struct binding *) hash_table_get(ht, 0, sym);
    if (binding == SENTINEL) {
        binding = malloc(sizeof(struct binding));
        hash_table_set(ht, 0, sym, binding);
    }
    binding->value = val;
    binding->kind = kind;
}

value env_ref(value e, value sym) {
    struct hash_table *ht = GET_OBJECT(e)->environment.hash_table;
    struct symbol *s = GET_SYMBOL(sym);
    if (ht == NULL) {
        return global_env_ref(sym);
    }

    struct binding *binding = (struct binding *) hash_table_get(ht, 0, sym);
    if (binding == SENTINEL) {
        RAISE("unbound variable: %.*s", (int) s->name_len, s->name);
    }

    switch (binding->kind) {
    case sym_unbound:
        RAISE("unbound variable: %.*s", (int) s->name_len, s->name);
    case sym_macro:
        RAISE("invalid use of macro: %.*s", (int) s->name_len, s->name);
    case sym_special:
        RAISE("invalid use of special: %.*s", (int) s->name_len, s->name);
    case sym_aux:
        RAISE("invalid use of aux keyword: %.*s", (int) s->name_len, s->name);
    case sym_value:
        return binding->value;
    case sym_alias:
        return env_ref(e, binding->value);
    case sym_primcall:
        /* same trick as the sentinel case above: binding->value is the
         * canonical primcall name symbol, whose own ->value already holds
         * a real closure from program init. */
        return GET_SYMBOL(binding->value)->value;
    default:
        RAISE("internal error: unhandled sym_kind case");
    }
}

/************ global environment functions ***********/

void init_symbols(void) {
    hash_table_init(&symbols, 128, symbol_name_hash, symbol_name_eq);
}

value extend_global_env(char *name, size_t name_len, enum sym_kind kind) {
    struct symbol_name *k = malloc(sizeof(struct symbol_name) + name_len);
    k->len = name_len;
    memcpy(k->name, name, name_len);
    value existing = hash_table_get(&symbols, 0, k);
    if (existing != SENTINEL) {
        free(k);
        return existing;
    }
    value sym = make_symbol(name, name_len, kind);
    hash_table_set(&symbols, 0, k, sym);
    return sym;
}

value make_global_env(void) {
    struct object *obj = malloc(sizeof(struct object));
    obj->type = OBJ_ENVIRONMENT;
    obj->environment.hash_table = NULL;
    return OBJECT(obj);
}

/************ port init functions ***********/

void init_ports() {
    current_input_port.type = OBJ_PORT;
    current_input_port.port.direction = PORT_DIR_READ;
    current_input_port.port.fp = stdin;
    current_input_port.port.read_char = file_read_char;
    current_input_port.port.peek_char = file_peek_char;
    current_input_port.port.read_line = file_read_line;
    current_input_port.port.unread_char = file_unread_char;

    current_output_port.type = OBJ_PORT;
    current_output_port.port.direction = PORT_DIR_WRITE;
    current_output_port.port.fp = stdout;
    current_output_port.port.printf = file_printf;
    current_output_port.port.write_char = file_write_char;

    current_error_port.type = OBJ_PORT;
    current_error_port.port.direction = PORT_DIR_WRITE;
    current_error_port.port.fp = stderr;
    current_error_port.port.printf = file_printf;
    current_error_port.port.write_char = file_write_char;
}

/************ static library registration ***********/

static struct static_lib *lib_list = NULL;

void register_static_lib(struct static_lib *lib) {
    lib->next = lib_list;
    lib_list = lib;
}

void run_static_libs(value env) {
    for (struct static_lib *p = lib_list; p; p = p->next)
        p->init(env);
}

/************ primcall functions ***********/

value primcall_append(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs == 0) { return NIL; }
    init_args();

    /* copy every argument but the last onto a single growing list via a
       tail pointer; the last argument is shared as the tail (and may be
       any object, giving an improper result). */
    value head = NIL;
    value tail = NIL;
    for (int i = 0; i < nargs; ++i) {
        value arg = next_arg();
        if (i == nargs - 1) {
            if (head == NIL) {
                head = arg;
            } else {
                GET_PAIR(tail)->cdr = arg;
            }
        } else {
            if (!is_proper_list(arg)) { RAISE("append: not a proper list"); }
            for (value p = arg; p != NIL; p = GET_PAIR(p)->cdr) {
                value cell = make_pair(GET_PAIR(p)->car, NIL);
                if (head == NIL) {
                    head = cell;
                } else {
                    GET_PAIR(tail)->cdr = cell;
                }

                tail = cell;
            }
        }
    }

    free_args();
    return head;
}

value primcall_apply(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 2) { RAISE("apply needs a procedure and a list argument"); }
    init_args();

    value func = next_arg();
    if (!IS_CLOSURE(func)) { RAISE("apply first argument is not a procedure"); }

    /* the arguments arrive through a va_list, so we must read the leading
       fixed args before we can reach the trailing list and learn the total
       count. hold them on the C stack (which the conservative scan covers)
       until the vector below exists. */
    int n_pre = nargs - 2;
    value pre[n_pre > 0 ? n_pre : 1];
    for (int i = 0; i < n_pre; ++i) {
        pre[i] = next_arg();
    }

    value args_list = next_arg();
    if (!IS_PAIR(args_list) && args_list != NIL) { RAISE("apply last argument is not a list"); }

    int list_len = 0;
    for (value p = args_list; p != NIL; p = GET_PAIR(p)->cdr) { ++list_len; }
    int func_nargs = n_pre + list_len;

    /* build the call's arguments in a GC vector. it is a normal heap object,
       so nothing here mallocs or frees and a non-local exit leaks nothing. */
    value argvec = make_vector(func_nargs, VOID);
    value *args = GET_OBJECT(argvec)->vector.data;
    for (int i = 0; i < n_pre; ++i) {
        args[i] = pre[i];
    }
    int i = n_pre;
    for (value p = args_list; p != NIL; p = GET_PAIR(p)->cdr) {
        args[i++] = GET_PAIR(p)->car;
    }

    /* `args` is an interior pointer into argvec's buffer and is
     * invisible to the conservative scan. the callee might allocate
     * (and trigger GC), so argvec must stay findable on this frame for
     * its whole duration: the keep_alive after the call stops the
     * optimizer from dropping argvec once `args` has been derived,
     * which would let the sweep free the buffer out from under the
     * callee. nothing allocates between here and the call, so there is
     * no earlier window to guard. */
    value ret = GET_CLOSURE(func)->func(GET_CLOSURE(func)->freevars, CALL_HAS_ARG_ARRAY, func_nargs, args);
    volatile value keep_alive = argvec;
    (void) keep_alive;

    free_args();
    return ret;
}

value primcall_boolean_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("boolean? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_BOOL(v));
}

value primcall_box(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("box needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    struct object *box = alloc_object();
    box->type = OBJ_BOX;
    box->box.value = v;
    return OBJECT(box);
}

value primcall_box_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("box? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_BOX(v));
}

value primcall_car(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("car needs a single argument"); }
    init_args();
    value arg = next_arg();
    free_args();
    if (!IS_PAIR(arg)) { RAISE("car argument is not a pair") }
    return GET_PAIR(arg)->car;
}

value primcall_cdr(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("car needs a single argument"); }
    init_args();
    value arg = next_arg();
    free_args();
    if (!IS_PAIR(arg)) { RAISE("cdr argument is not a pair") }
    return GET_PAIR(arg)->cdr;
}

value primcall_char_downcase(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("char-downcase needs a single argument"); }
    init_args();
    value ch = next_arg();
    free_args();
    if (!IS_CHAR(ch)) { RAISE("char-downcase argument is not a char") }
    return GET_CHAR(ch) >= 'A' && GET_CHAR(ch) <= 'Z' ? CHAR(GET_CHAR(ch) - 'A' + 'a') : ch;
}

value primcall_char_upcase(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("char-upcase needs a single argument"); }
    init_args();
    value ch = next_arg();
    free_args();
    if (!IS_CHAR(ch)) { RAISE("char-upcase argument is not a char") }
    return GET_CHAR(ch) >= 'a' && GET_CHAR(ch) <= 'z' ? CHAR(GET_CHAR(ch) - 'a' + 'A') : ch;
}

value primcall_char_to_integer(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("char->integer needs a single argument"); }
    init_args();
    value ch = next_arg();
    free_args();
    if (!IS_CHAR(ch)) { RAISE("char->integer argument is not a char") }
    return FIXNUM((int)(uint8_t) GET_CHAR(ch));
}

value primcall_char_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("char? needs a single argument"); }
    init_args();
    value x = next_arg();
    free_args();
    return BOOL(IS_CHAR(x));
}

value primcall_close_port(environment env, enum call_flags flags, int nargs, ...) {
    init_args();
    value port = next_arg();
    free_args();
    if (!IS_PORT(port)) { RAISE("close-port argument is not a port") }
    if (GET_OBJECT(port)->port.closed) return VOID;
    int ret = fclose(GET_OBJECT(port)->port.fp);
    if (ret) { RAISE("failed to close the port: %s", strerror(errno)); }
    GET_OBJECT(port)->port.closed = 1;
    return VOID;
}

value primcall_cons(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("cons needs two arguments"); }
    init_args();
    value car = next_arg();
    value cdr = next_arg();
    free_args();
    return make_pair(car, cdr);
}

value primcall_command_line(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("cons needs two arguments"); }

    value cmdline = NIL;
    for (int i = cmdline_argc - 1; i >= 0; --i) {
        value s = make_string(cmdline_argv[i], strlen(cmdline_argv[i]));
        cmdline = make_pair(s, cmdline);
    }

    return cmdline;
}

value primcall_current_error_port(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("current-error-port needs no arguments"); }
    return OBJECT(&current_error_port);
}

value primcall_current_input_port(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("current-input-port needs no arguments"); }
    return OBJECT(&current_input_port);
}

value primcall_current_output_port(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("current-output-port needs no arguments"); }
    return OBJECT(&current_output_port);
}

value primcall_delete_file(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("delete-file needs a single argument"); }
    init_args();
    value filename = next_arg();
    if (!IS_STRING(filename)) { RAISE("delete-file argument is not a string"); }
    free_args();

    char *filenamez = strz(filename);
    int ret = unlink(filenamez);
    free(filenamez);

    if (ret) {
        struct object *err = alloc_object();
        err->type = OBJ_ERROR;
        err->error.type = ERR_FILE;
        err->error.err_no = errno;
        return OBJECT(err);
    }

    return VOID;
}

value primcall_display(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("display needs one or two arguments"); }
    init_args();
    value v = next_arg();
    value port = nargs == 1 ? OBJECT(&current_output_port) : next_arg();
    free_args();
    if (!IS_PORT(port)) { RAISE("writing to non-port"); }
    if (GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("writing to non-output port"); }
    _display(v, port);
    return VOID;
}

value primcall_eof_object_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("eof-object? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_EOFOBJ(v));
}

value primcall_eq_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("eq? needs two arguments"); }
    init_args();
    value v1 = next_arg();
    value v2 = next_arg();
    free_args();
    return BOOL(v1 == v2);
}

value primcall_error(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("error needs a single argument"); }
    init_args();
    value msg = next_arg();
    free_args();
    if (!IS_STRING(msg)) { RAISE("error argument is not a string"); }
    fprintf(stderr, "error: ");
    _display(msg, OBJECT(&current_error_port));
    fprintf(stderr, "\n");
    print_stacktrace();
    cleanup();
    exit(1);
    return VOID;
}

value primcall_error_object_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("error-object? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_ERROR);
}

value primcall_exit(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("exit needs zero or one argument"); }
    init_args();
    value code = nargs == 1 ? next_arg() : FIXNUM(0);
    free_args();
    if (IS_BOOL(code)) {
        cleanup();
        if (GET_BOOL(code))
            exit(0);
        else
            exit(1);
    } else if (IS_FIXNUM(code)) {
        cleanup();
        exit(GET_FIXNUM(code));
    } else {
        RAISE("invalid exit code");
    }

    return VOID;
}

value primcall_file_error_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("file-error? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_ERROR && GET_OBJECT(v)->error.type == ERR_FILE);
}

value primcall_gensym(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("gensym needs zero or one argument"); }
    init_args();

    struct symbol *sym;

    if (nargs == 1) {
        value name = next_arg();
        if (!IS_STRING(name)) { RAISE("gensym argument is not a string"); }
        uint64_t n = gensym_counter++;
        int prefix_len = (int)GET_STRING(name)->len;
        int suffix_len = snprintf(NULL, 0, "%lu", n);
        int len = prefix_len + suffix_len;
        char *sym_name = malloc(len + 1); /* plus one for NULL terminator */
        memcpy(sym_name, GET_STRING(name)->s, prefix_len);
        snprintf(sym_name + prefix_len, suffix_len + 1, "%lu", n);
        sym = make_symbol(sym_name, len, sym_unbound);
    } else {
        char buf[32];
        snprintf(buf, sizeof(buf), "g%lu", gensym_counter++);
        int len = strlen(buf);
        char *name = malloc(len);
        memcpy(name, buf, len);
        sym = make_symbol(name, len, sym_unbound);
    }

    free_args();

    return SYMBOL(sym);
}

value primcall_get_environment_variable(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("get-environment-variable needs a single argument"); }
    init_args();
    value name = next_arg();
    if (!IS_STRING(name)) { RAISE("get-environment-variable argument is not a string"); }
    free_args();

    char *namez = strz(name);
    char *valz = getenv(namez);
    free(namez);

    value v;
    if (valz) {
        v = make_string(valz, strlen(valz));
    } else {
        v = FALSE;
    }

    return OBJECT(v);
}

value primcall_get_output_string(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("get-output-string needs a single argument"); }
    init_args();
    value port = next_arg();
    free_args();
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE || GET_OBJECT(port)->port.string == NULL) { RAISE("argument is not an output string port"); }
    return make_string(GET_OBJECT(port)->port.string, GET_OBJECT(port)->port.string_len);
}

value primcall_input_port_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("input-port? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_PORT(v) && GET_OBJECT(v)->port.direction == PORT_DIR_READ);
}

value primcall_integer_to_char(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("integer->char needs a single argument"); }
    init_args();
    value n = next_arg();
    free_args();
    if (!IS_FIXNUM(n)) { RAISE("integer->char argument is not a number") }
    if (GET_FIXNUM(n) < 0 || GET_FIXNUM(n) > 255) { RAISE("integer->char argument is out of range") }
    return CHAR((char) GET_FIXNUM(n));
}

value primcall_list(environment env, enum call_flags flags, int nargs, ...) {
    init_args();
    value result = NIL;
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        result = make_pair(v, result);
    }
    free_args();

    return reverse_list(result, NIL);
}

value primcall_list_star(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs == 0) { RAISE("list* needs at least one argument"); }
    init_args();
    value rev = NIL;
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        rev = make_pair(v, rev);
    }
    free_args();

    value result = GET_PAIR(rev)->car;
    for (value p = GET_PAIR(rev)->cdr; p != NIL; p = GET_PAIR(p)->cdr) {
        result = make_pair(GET_PAIR(p)->car, result);
    }

    return result;
}

value primcall_list_to_vector(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("list->vector needs a single argument"); }
    init_args();
    value ls = next_arg();
    free_args();

    if (!is_proper_list(ls)) { RAISE("list->vector argument must be a list"); }

    size_t len = 0;
    for (value v = ls; v != NIL; v = GET_PAIR(v)->cdr) { ++len; }

    value vec = make_vector(len, VOID);
    size_t i = 0;
    for (value v = ls; v != NIL; v = GET_PAIR(v)->cdr) {
        GET_OBJECT(vec)->vector.data[i++] = GET_PAIR(v)->car;
    }

    return vec;
}

value primcall_make_string(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("make-string needs one or two arguments"); }
    init_args();
    value n = next_arg();
    value ch = nargs == 1 ? CHAR(0) : next_arg();
    free_args();
    if (!IS_FIXNUM(n)) { RAISE("make-string first argument should be a number"); }
    if (GET_FIXNUM(n) < 0) { RAISE("make-string first argument is negative"); }
    if (!IS_CHAR(ch)) { RAISE("make-string second argument should be a character"); }
    return STRING(alloc_string(GET_FIXNUM(n), GET_CHAR(ch)));
}

value primcall_make_vector(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("make-vector needs one or two arguments"); }
    init_args();
    value n = next_arg();
    value fill = nargs == 1 ? VOID : next_arg();
    free_args();
    if (!IS_FIXNUM(n)) { RAISE("make-vector first argument should be a number"); }
    if (GET_FIXNUM(n) < 0) { RAISE("make-vector first argument is negative"); }
    return make_vector(GET_FIXNUM(n), fill);
}

value primcall_newline(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("newline needs zero or one argument"); }
    init_args();
    value port = nargs == 1 ? next_arg() : OBJECT(&current_output_port);
    free_args();
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("newline argument is not an output port"); }
    GET_OBJECT(port)->port.write_char(port, CHAR('\n'));
    return VOID;
}

value primcall_not(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("not needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();

    return BOOL(v == FALSE);
}

value primcall_null_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("null? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();

    return BOOL(v == NIL);
}

value primcall_number_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("number? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_FIXNUM(v));
}

value primcall_number_to_string(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("number->string needs one or two arguments"); }
    init_args();
    value n = next_arg();
    value base = nargs == 1 ? FIXNUM(10) : next_arg();
    free_args();
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

value primcall_open_input_file(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("open-input-file needs a single argument"); }
    init_args();
    value filename = next_arg();
    free_args();
    if (!IS_STRING(filename)) { RAISE("filename is not a string"); }
    struct object *obj = alloc_object();
    int filename_len = GET_STRING(filename)->len;
    obj->port.filename = malloc(filename_len + 1);
    snprintf(obj->port.filename, filename_len + 1, "%.*s", filename_len, GET_STRING(filename)->s);
    FILE *fp = fopen(obj->port.filename, "r");
    if (!fp) { RAISE("error opening file '%s': %s", obj->port.filename, strerror(errno)); }

    obj->type = OBJ_PORT;
    obj->port.direction = PORT_DIR_READ;
    obj->port.fp = fp;
    obj->port.read_char = file_read_char;
    obj->port.peek_char = file_peek_char;
    obj->port.read_line = file_read_line;
    obj->port.unread_char = file_unread_char;
    return OBJECT(obj);
}

value primcall_open_output_file(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("open-output-file needs a single argument"); }
    init_args();
    value filename = next_arg();
    free_args();
    if (!IS_STRING(filename)) { RAISE("filename is not a string"); }
    char *filenamez = strz(GET_STRING(filename));
    FILE *fp = fopen(filenamez, "w");
    if (!fp) { RAISE("error opening file '%s': %s", filenamez, strerror(errno)); }
    free(filenamez);
    struct object *obj = alloc_object();
    obj->type = OBJ_PORT;
    obj->port.direction = PORT_DIR_WRITE;
    obj->port.fp = fp;
    obj->port.printf = file_printf;
    obj->port.write_char = file_write_char;
    return OBJECT(obj);
}

value primcall_open_output_string(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("open-output-string accepts no arguments"); }
    struct object *obj = alloc_object();
    obj->type = OBJ_PORT;
    obj->port.direction = PORT_DIR_WRITE;
    obj->port.string = malloc(128);
    obj->port.string_cap = 128;
    obj->port.string_len = 0;
    obj->port.printf = string_printf;
    obj->port.write_char = string_write_char;
    return OBJECT(obj);
}

value primcall_pair_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("port? needs a single argument"); }
    init_args();
    value x = next_arg();
    free_args();
    return BOOL(IS_PAIR(x));
}

value primcall_peek_char(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("peek-char needs zero or one argument"); }
    init_args();
    value port = nargs == 1 ? next_arg() : OBJECT(&current_input_port);
    free_args();
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE("peek-char argument is not an input port"); }
    return GET_OBJECT(port)->port.peek_char(port);
}

value primcall_port_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("port? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_PORT(v));
}

value primcall_procedure_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("procedure? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_CLOSURE(v));
}

value primcall_read_char(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("read-char needs zero or one argument"); }
    init_args();
    value port = nargs == 1 ? next_arg() : OBJECT(&current_input_port);
    free_args();
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE("read-char argument is not an input port"); }
    return GET_OBJECT(port)->port.read_char(port);
}

value primcall_read_line(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0 && nargs != 1) { RAISE("read-line needs zero or one argument"); }
    init_args();
    value port = nargs == 1 ? next_arg() : OBJECT(&current_input_port);
    free_args();
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE("read-line argument is not an input port"); }
    return GET_OBJECT(port)->port.read_line(port);
}

value primcall_set_box_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("set-box! needs two arguments"); }
    init_args();
    value box = next_arg();
    value obj = next_arg();
    free_args();

    if (!IS_BOX(box)) { RAISE("set-box! first argument is not a box"); }
    GET_OBJECT(box)->box.value = obj;
    return VOID;
}

value primcall_set_car_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("set-car! needs two arguments"); }
    init_args();
    value pair = next_arg();
    value obj = next_arg();
    free_args();

    if (!IS_PAIR(pair)) { RAISE("set-car! first argument is not a pair"); }
    GET_PAIR(pair)->car = obj;
    return VOID;
}

value primcall_set_cdr_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("set-cdr! needs two arguments"); }
    init_args();
    value pair = next_arg();
    value obj = next_arg();
    free_args();

    if (!IS_PAIR(pair)) { RAISE("set-cdr! first argument is not a pair"); }
    GET_PAIR(pair)->cdr = obj;
    return VOID;
}

value primcall_string_to_number(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("string-to-number needs one or two arguments"); }
    init_args();
    value str_v = next_arg();
    value base = nargs == 1 ? FIXNUM(10) : next_arg();
    free_args();
    if (!IS_STRING(str_v)) { RAISE("string->number first argument must be a string"); }
    if (!IS_FIXNUM(base)) { RAISE("string->number second argument must be a number"); }
    if (GET_STRING(str_v)->len == 0) return FALSE;

    char *str = strz(str_v);
    char *endptr;
    int64_t result = strtoll(str, &endptr, GET_FIXNUM(base));
    if (endptr != str + GET_STRING(str_v)->len) { free(str); return FALSE; }
    free(str);
    return FIXNUM(result);
}

value primcall_string_to_symbol(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("string->symbol needs a single argument"); }
    init_args();
    value str = next_arg();
    free_args();
    if (!IS_STRING(str)) { RAISE("string->symbol argument is not a string"); }
    return string_to_symbol(str);
}

value primcall_symbol_to_string(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("symbol->string needs a single argument"); }
    init_args();
    value sym = next_arg();
    free_args();
    if (!IS_SYMBOL(sym)) { RAISE("symbol->string argument is not a symbol"); }
    return symbol_to_string(sym);
}

value primcall_string_append(environment env, enum call_flags flags, int nargs, ...) {
    int total_size = 0;
    init_args();
    for (int i = 0; i < nargs; ++i) {
        value arg = next_arg();
        if (!IS_STRING(arg)) { RAISE("string-append argument is not a string"); }

        struct string *str = GET_STRING(arg);
        total_size += str->len;
    }

    struct string *concat = alloc_string(total_size, '\0');

    reset_args();
    size_t offset = 0;
    for (int i = 0; i < nargs; ++i) {
        struct string *str = GET_STRING(next_arg());
        memcpy(concat->s + offset, str->s, str->len);
        offset += str->len;
    }
    free_args();

    return STRING(concat);
}

value primcall_string_copy(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs > 3) { RAISE("string-copy needs at most three arguments"); }
    init_args();
    value str = next_arg();
    if (!IS_STRING(str)) { RAISE("string-copy first argument is not a string"); }
    value start = nargs > 1 ? next_arg() : FIXNUM(0);
    value end = nargs > 2 ? next_arg() : FIXNUM(GET_STRING(str)->len);
    free_args();
    if (!IS_FIXNUM(start)) { RAISE("string-copy second argument is not a number"); }
    if (!IS_FIXNUM(end)) { RAISE("string-copy third argument is not a number"); }
    if (GET_FIXNUM(start) < 0 || GET_FIXNUM(start) >= GET_STRING(str)->len) { RAISE("string-copy start index is out of range"); }
    if (GET_FIXNUM(end) < 0 || GET_FIXNUM(end) > GET_STRING(str)->len) { RAISE("string-copy end index is out of range"); }
    struct string *result = alloc_string(GET_FIXNUM(end) - GET_FIXNUM(start), '\0');
    memcpy(result->s, GET_STRING(str)->s + GET_FIXNUM(start), result->len);
    return STRING(result);
}

value primcall_string_length(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("string-length needs a single argument"); }
    init_args();
    value str = next_arg();
    free_args();
    if (!IS_STRING(str)) { RAISE("string-length argument is not a string"); }
    return FIXNUM(GET_STRING(str)->len);
}

value primcall_string_ref(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("string-ref needs two arguments"); }
    init_args();
    value str = next_arg();
    value idx = next_arg();
    free_args();
    if (!IS_STRING(str)) { RAISE("string-ref first argument is not a string"); }
    if (!IS_FIXNUM(idx)) { RAISE("string-ref second argument is not a number"); }
    if (GET_FIXNUM(idx) < 0 || GET_FIXNUM(idx) >= GET_STRING(str)->len) { RAISE("string-ref index is out of range"); }
    return CHAR(GET_STRING(str)->s[GET_FIXNUM(idx)]);
}

value primcall_string_set_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { RAISE("string-set! needs three arguments"); }
    init_args();
    value str = next_arg();
    value idx = next_arg();
    value ch = next_arg();
    free_args();
    if (!IS_STRING(str)) { RAISE("string-set! first argument is not a string"); }
    if (!IS_FIXNUM(idx)) { RAISE("string-set! second argument is not a number"); }
    if (GET_FIXNUM(idx) < 0 || GET_FIXNUM(idx) >= GET_STRING(str)->len) { RAISE("string-set! index is out of range"); }
    if (!IS_CHAR(ch)) { RAISE("string-set! third argument is not a char"); }
    GET_STRING(str)->s[GET_FIXNUM(idx)] = GET_CHAR(ch);
    return VOID;
}

value primcall_string_eq_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 2) { RAISE("string=? needs at least two arguments"); }
    init_args();
    value prev = next_arg();
    if (!IS_STRING(prev)) { RAISE("string=? argument is not a string"); }
    for (int i = 1; i < nargs; ++i) {
        value cur = next_arg();
        if (!IS_STRING(cur)) { RAISE("string=? argument is not a string"); }
        if (string_cmp(GET_STRING(prev), GET_STRING(cur)) != 0) {
            free_args();
            return FALSE;
        }
        prev = cur;
    }

    free_args();
    return TRUE;
}

value primcall_string_ci_eq_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 2) { RAISE("string-ci=? needs at least two arguments"); }
    init_args();
    value prev = next_arg();
    if (!IS_STRING(prev)) { RAISE("string-ci=? argument is not a string"); }
    for (int i = 1; i < nargs; ++i) {
        value cur = next_arg();
        if (!IS_STRING(cur)) { RAISE("string-ci=? argument is not a string"); }
        if (string_ci_cmp(GET_STRING(prev), GET_STRING(cur)) != 0) {
            free_args();
            return FALSE;
        }
        prev = cur;
    }

    free_args();
    return TRUE;
}

value primcall_string_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("string? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_STRING(v));
}

value primcall_substring(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { RAISE("substring needs three arguments"); }
    init_args();
    value str = next_arg();
    value start = next_arg();
    value end = next_arg();
    free_args();
    if (!IS_STRING(str)) { RAISE("substring first argument is not a string"); }
    if (!IS_FIXNUM(start)) { RAISE("substring second argument is not a number"); }
    if (!IS_FIXNUM(end)) { RAISE("substring third argument is not a number"); }
    if (GET_FIXNUM(start) < 0 || GET_FIXNUM(start) >= GET_STRING(str)->len) { RAISE("substring start index is out of range"); }
    if (GET_FIXNUM(end) < 0 || GET_FIXNUM(end) > GET_STRING(str)->len) { RAISE("substring end index is out of range"); }
    struct string *result = alloc_string(GET_FIXNUM(end) - GET_FIXNUM(start), '\0');
    memcpy(result->s, GET_STRING(str)->s + GET_FIXNUM(start), result->len);
    return STRING(result);
}

value primcall_symbol_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("symbol? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_SYMBOL(v));
}

value primcall_system(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("system needs a single argument"); }
    init_args();
    value cmd = next_arg();
    free_args();
    if (!IS_STRING(cmd)) { RAISE("system argument is not a string"); }
    char *cmdz = strz(cmd);
    int ret = system(cmdz);
    free(cmdz);
    return FIXNUM(ret);
}

value primcall_unread_char(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("unread-char needs one or two arguments"); }
    init_args();
    value ch = next_arg();
    if (!IS_CHAR(ch)) { RAISE("unread-char first argument is not a character"); }
    value port = nargs == 2 ? next_arg() : OBJECT(&current_input_port);
    if (!IS_PORT(port)) { RAISE("unread-char second argument is not a port"); }
    free_args();
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_READ) { RAISE("unread-char argument is not an input port"); }
    GET_OBJECT(port)->port.unread_char(port, ch);
    return VOID;
}

value primcall_urandom(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("urandom needs a single argument"); }
    init_args();
    value n = next_arg();
    if (!IS_FIXNUM(n)) { RAISE("urandom argument is not a number"); }
    free_args();

    FILE *fp = fopen("/dev/urandom", "r");
    value s = STRING(alloc_string(GET_FIXNUM(n), '\0'));
    int nread = fread(GET_STRING(s)->s, 1, GET_FIXNUM(n), fp);
    if (nread != GET_FIXNUM(n)) { RAISE("could not read enough bytes from /dev/urandom"); }

    return s;
}

value primcall_unbox(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("unbox needs a single argument"); }
    init_args();
    value box = next_arg();
    free_args();

    if (!IS_BOX(box)) { RAISE("unbox argument is not a box object"); }
    return GET_OBJECT(box)->box.value;
}

value primcall_unwrap(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("unwrap needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();

    if (!IS_WRAPPED(v)) { RAISE("unwrap argument is not a wrapped object"); }
    return GET_OBJECT(v)->wrapped.value;
}

value primcall_vector_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("vector? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_VECTOR(v));
}

value primcall_vector_length(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("vector-length needs a single argument"); }
    init_args();
    value vec = next_arg();
    free_args();
    if (!IS_VECTOR(vec)) { RAISE("vector-length argument is not a vector"); }
    return FIXNUM(GET_OBJECT(vec)->vector.len);
}

value primcall_vector_ref(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("vector-ref needs two arguments"); }
    init_args();
    value vec = next_arg();
    value idx = next_arg();
    free_args();
    if (!IS_VECTOR(vec)) { RAISE("vector-ref first argument is not a vector"); }
    if (!IS_FIXNUM(idx)) { RAISE("vector-ref second argument is not a number"); }
    if (GET_FIXNUM(idx) < 0 || GET_FIXNUM(idx) >= GET_OBJECT(vec)->vector.len) { RAISE("vector-ref index is out of range"); }
    return GET_OBJECT(vec)->vector.data[GET_FIXNUM(idx)];
}

value primcall_vector_set_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { RAISE("vector-set! needs three arguments"); }
    init_args();
    value vec = next_arg();
    value idx = next_arg();
    value v = next_arg();
    free_args();
    if (!IS_VECTOR(vec)) { RAISE("vector-set! first argument is not a vector"); }
    if (!IS_FIXNUM(idx)) { RAISE("vector-set! second argument is not a number"); }
    if (GET_FIXNUM(idx) < 0 || GET_FIXNUM(idx) >= GET_OBJECT(vec)->vector.len) { RAISE("vector index is out of range"); }
    GET_OBJECT(vec)->vector.data[GET_FIXNUM(idx)] = v;
    return VOID;
}

value primcall_void(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("void accepts no arguments"); }
    return VOID;
}

value primcall_void_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("void? accepts a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_VOID(v));
}

value primcall_wrap(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("wrap needs two arguments"); }
    init_args();
    value v = next_arg();
    value kind = next_arg();
    free_args();

    struct object *w = alloc_object();
    w->type = OBJ_WRAPPED;
    w->wrapped.value = v;
    w->wrapped.kind = kind;
    return OBJECT(w);
}

value primcall_wrapped_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("wrapped? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_WRAPPED(v));
}

value primcall_wrapped_kind(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("wrapped-kind needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();

    if (!IS_WRAPPED(v)) { RAISE("wrapped-kind argument is not a wrapped object"); }
    return GET_OBJECT(v)->wrapped.kind;
}

value primcall_wrapped_set_print(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("wrapped-set-print needs two arguments"); }
    init_args();
    value kind = next_arg();
    value proc = next_arg();
    free_args();

    if (!IS_CLOSURE(proc)) { RAISE("wrapped-set-print second argument not a procedure"); }

    n_wrapped_print_procs++;
    wrapped_print_procs = realloc(wrapped_print_procs, n_wrapped_print_procs * sizeof(struct kind_proc));
    wrapped_print_procs[n_wrapped_print_procs - 1].kind = kind;
    wrapped_print_procs[n_wrapped_print_procs - 1].proc = proc;

    return VOID;
}

value primcall_write(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("write needs one or two arguments"); }
    init_args();
    value v = next_arg();
    value port = nargs == 1 ? OBJECT(&current_output_port) : next_arg();
    free_args();
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("write second argument is not an output port"); }
    _write(v, port);
    return VOID;
}

value primcall_write_char(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("write-char needs one or two arguments"); }
    init_args();
    value ch = next_arg();
    value port = nargs == 1 ? OBJECT(&current_output_port) : next_arg();
    free_args();
    if (!IS_CHAR(ch)) { RAISE("write-char first argument is not a char"); }
    if (!IS_PORT(port) || GET_OBJECT(port)->port.direction != PORT_DIR_WRITE) { RAISE("write-char second argument is not an output port"); }
    GET_OBJECT(port)->port.write_char(port, ch);
    return VOID;
}

value primcall_add(environment env, enum call_flags flags, int nargs, ...) {
    value result = FIXNUM(0);
    init_args();
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        if (!IS_FIXNUM(v)) { RAISE("addition (+) argument is not a number") }
        result += (int64_t) v;
    }
    return result;
}

value primcall_div(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 1) { RAISE("division (-) needs at least one argument"); }
    init_args();
    value result_v = next_arg();
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
        value v = next_arg();
        if (!IS_FIXNUM(v)) { RAISE("division (/) argument is not a number") }
        if (GET_FIXNUM(v) == 0)
            RAISE("division by zero");
        result /= GET_FIXNUM(v);
    }

    free_args();
    return FIXNUM(result);
}

value primcall_mul(environment env, enum call_flags flags, int nargs, ...) {
    int64_t result = 1;
    init_args();
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        if (!IS_FIXNUM(v)) { RAISE("multiplication (*) argument is not a number") }
        result *= GET_FIXNUM(v);
    }

    free_args();
    return FIXNUM(result);
}

value primcall_sub(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 1) { RAISE("subtraction (-) needs at least one argument"); }
    init_args();
    value result = next_arg();
    if (!IS_FIXNUM(result)) { RAISE("subtraction (-) argument is not a number"); }
    if (nargs == 1) return FIXNUM(-GET_FIXNUM(result));
    for (int i = 1; i < nargs; ++i) {
        value v = next_arg();
        if (!IS_FIXNUM(v)) { RAISE("subtraction (-) argument is not a number") }
        result -= (int64_t) v;
    }

    free_args();
    return result;
}

value primcall_num_eq(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 1) { RAISE("= needs at least one argument"); }
    init_args();
    value n = next_arg();
    if (!IS_FIXNUM(n)) { RAISE("= argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = next_arg();
        if (!IS_FIXNUM(m)) { RAISE("= argument is not a number") }
        if (GET_FIXNUM(n) != GET_FIXNUM(m)) return FALSE;
    }

    free_args();
    return TRUE;
}

value primcall_num_lt(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 1) { RAISE("< needs at least one argument"); }
    init_args();
    value n = next_arg();
    if (!IS_FIXNUM(n)) { RAISE("< argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = next_arg();
        if (!IS_FIXNUM(m)) { RAISE("< argument is not a number") }
        if (GET_FIXNUM(n) >= GET_FIXNUM(m)) return FALSE;
    }

    free_args();
    return TRUE;
}

value primcall_num_gt(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 1) { RAISE("> needs at least one argument"); }
    init_args();
    value n = next_arg();
    if (!IS_FIXNUM(n)) { RAISE("> argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = next_arg();
        if (!IS_FIXNUM(m)) { RAISE("> argument is not a number") }
        if (GET_FIXNUM(n) <= GET_FIXNUM(m)) return FALSE;
    }

    free_args();
    return TRUE;
}

value primcall_num_le(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 1) { RAISE("<= needs at least one argument"); }
    init_args();
    value n = next_arg();
    if (!IS_FIXNUM(n)) { RAISE("<= argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = next_arg();
        if (!IS_FIXNUM(m)) { RAISE("<= argument is not a number") }
        if (GET_FIXNUM(n) > GET_FIXNUM(m)) return FALSE;
    }

    free_args();
    return TRUE;
}

value primcall_num_ge(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs < 1) { RAISE(">= needs at least one argument"); }
    init_args();
    value n = next_arg();
    if (!IS_FIXNUM(n)) { RAISE(">= argument is not a number"); }
    for (int i = 1; i < nargs; ++i) {
        value m = next_arg();
        if (!IS_FIXNUM(m)) { RAISE(">= argument is not a number") }
        if (GET_FIXNUM(n) < GET_FIXNUM(m)) return FALSE;
    }

    free_args();
    return TRUE;
}

static uint64_t hash_fn_wrapper(struct hash_table *ht, value key) {
    struct closure *c = GET_CLOSURE(ht->user_hash_fn);
    value result = c->func(c->freevars, NO_CALL_FLAGS, 1, key);
    return (uint64_t) GET_FIXNUM(result);
}

static int eq_fn_wrapper(struct hash_table *ht, value a, value b) {
    struct closure *c = GET_CLOSURE(ht->user_eq_fn);
    return c->func(c->freevars, NO_CALL_FLAGS, 2, a, b) != FALSE;
}

value primcall_percent_make_hash_table(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("%%make-hash-table accepts two arguments"); }
    init_args();
    value eq_fn = next_arg();
    value hash_fn = next_arg();
    free_args();

    if (eq_fn != FALSE && (!IS_CLOSURE(eq_fn) || !CLOSURE_ACCEPTS(GET_CLOSURE(eq_fn), 2))) {
        RAISE("%%make-hash-table first argument must be #f or a procedure that accepts two arguments");
    }

    if (hash_fn != FALSE && (!IS_CLOSURE(hash_fn) || !CLOSURE_ACCEPTS(GET_CLOSURE(hash_fn), 1))) {
        RAISE("%%make-hash-table second argument must be #f or a procedure that accepts a single argument");
    }

    struct object *obj = alloc_object();
    obj->type = OBJ_HASH_TABLE;
    hash_table_init(&obj->hash_table.ht,
                    HASH_TABLE_INITIAL_SIZE,
                    hash_fn == FALSE ? NULL : hash_fn_wrapper,
                    eq_fn == FALSE ? NULL : eq_fn_wrapper);
    obj->hash_table.ht.user_hash_fn = hash_fn;
    obj->hash_table.ht.user_eq_fn = eq_fn;
    return OBJECT(obj);
}

value primcall_hash_table_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_HASH_TABLE(v));
}

value primcall_hash_table_set_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { RAISE("hash-table-set! needs three arguments"); }
    init_args();
    value ht = next_arg();
    value k = next_arg();
    value v = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-set! first argument is not a hash-table"); }
    hash_table_set(&GET_OBJECT(ht)->hash_table.ht, ht, k, v);
    return VOID;
}

value primcall_hash_table_delete_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("hash-table-delete! needs two arguments"); }
    init_args();
    value ht = next_arg();
    value k = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-delete! first argument is not a hash-table"); }
    hash_table_delete(&GET_OBJECT(ht)->hash_table.ht, ht, k);
    return VOID;
}

value primcall_hash_table_ref(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2 && nargs != 3) { RAISE("hash-table-ref needs two or three arguments"); }
    init_args();
    value ht = next_arg();
    value k = next_arg();
    value thunk = nargs == 3 ? next_arg() : FALSE;
    free_args();

    if (thunk != FALSE) {
        if (!IS_CLOSURE(thunk)) {
            RAISE("thunk is not a procedure");
        }

        if (!CLOSURE_ACCEPTS(GET_CLOSURE(thunk), 0)) {
            RAISE("thunk should accept 0 arguments");
        }
    }

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-ref first argument is not a hash-table"); }
    value v = hash_table_get(&GET_OBJECT(ht)->hash_table.ht, ht, k);

    if (v == SENTINEL) {
        if (thunk == FALSE) {
            RAISE("key not found in hash-table");
        } else {
            struct closure *c = GET_CLOSURE(thunk);
            return c->func(c->freevars, NO_CALL_FLAGS, 0);
        }
    }

    return v;
}

value primcall_hash_table_ref_default(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { RAISE("hash-table-ref/default needs three arguments"); }
    init_args();
    value ht = next_arg();
    value k = next_arg();
    value deflt = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-ref/default first argument is not a hash-table"); }
    value v = hash_table_get(&GET_OBJECT(ht)->hash_table.ht, ht, k);

    if (v == SENTINEL) {
        return deflt;
    }

    return v;
}

value primcall_hash_table_exists_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("hash-table-exists? needs two arguments"); }
    init_args();
    value ht = next_arg();
    value k = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-exists? first argument is not a hash-table"); }
    value v = hash_table_get(&GET_OBJECT(ht)->hash_table.ht, ht, k);
    return BOOL(v != SENTINEL);
}

value primcall_hash_table_size(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table-size needs a single argument"); }
    init_args();
    value ht = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-size argument is not a hash-table"); }
    return FIXNUM(GET_OBJECT(ht)->hash_table.ht.size);
}

value primcall_hash_table_update_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3 && nargs != 4) { RAISE("hash-table-update! needs three or four arguments"); }
    init_args();
    value ht = next_arg();
    value k = next_arg();
    value func = next_arg();
    value thunk = nargs == 4 ? next_arg() : FALSE;
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-update! first argument is not a hash-table"); }

    if (!IS_CLOSURE(func)) {
        RAISE("hash-table-update! third argument (function) is not a procedure");
    }
    if (!CLOSURE_ACCEPTS(GET_CLOSURE(func), 1)) {
        RAISE("hash-table-update! third argument (function) procedure should accept a single argument");
    }

    if (thunk != FALSE) {
        if (!IS_CLOSURE(thunk)) {
            RAISE("hash-table-update! fourth argument (thunk) is not a procedure");
        }

        if (!CLOSURE_ACCEPTS(GET_CLOSURE(thunk), 0)) {
            RAISE("hash-table-update! fourth argument (thunk) should accept 0 arguments");
        }
    }

    value v = hash_table_get(&GET_OBJECT(ht)->hash_table.ht, ht, k);
    if (v == SENTINEL) {
        if (thunk == FALSE) {
            RAISE("key not found in hash-table");
        }

        struct closure *c = GET_CLOSURE(thunk);
        v = c->func(c->freevars, NO_CALL_FLAGS, 0);
    }

    struct closure *c = GET_CLOSURE(func);
    v = c->func(c->freevars, NO_CALL_FLAGS, 1, v);

    hash_table_set(&GET_OBJECT(ht)->hash_table.ht, ht, k, v);

    return VOID;
}

value primcall_hash_table_update_b_default(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 4) { RAISE("hash-table-update!/default needs four arguments"); }
    init_args();
    value ht = next_arg();
    value k = next_arg();
    value func = next_arg();
    value deflt = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-update!/default first argument is not a hash-table"); }

    if (!IS_CLOSURE(func)) {
        RAISE("hash-table-update!/default third argument (function) is not a procedure");
    }
    if (!CLOSURE_ACCEPTS(GET_CLOSURE(func), 1)) {
        RAISE("hash-table-update!/default hash-table-update!/default argument (function) procedure should accept a single argument");
    }

    value v = hash_table_get(&GET_OBJECT(ht)->hash_table.ht, ht, k);
    if (v == SENTINEL) {
        v = deflt;
    }

    struct closure *c = GET_CLOSURE(func);
    v = c->func(c->freevars, NO_CALL_FLAGS, 1, v);

    hash_table_set(&GET_OBJECT(ht)->hash_table.ht, ht, k, v);

    return VOID;
}

static void keys_accummulator(value k, value v, void *ctx) {
    value *list = (value *) ctx;
    *list = make_pair(k, *list);
}

value primcall_hash_table_keys(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table-keys needs a single argument"); }
    init_args();
    value ht = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-keys argument is not a hash-table"); }

    value list = NIL;
    hash_table_each(&GET_OBJECT(ht)->hash_table.ht, keys_accummulator, &list);

    return list;
}

static void values_accummulator(value k, value v, void *ctx) {
    value *list = (value *) ctx;
    *list = make_pair(v, *list);
}

value primcall_hash_table_values(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table-values needs a single argument"); }
    init_args();
    value ht = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-values argument is not a hash-table"); }

    value list = NIL;
    hash_table_each(&GET_OBJECT(ht)->hash_table.ht, values_accummulator, &list);

    return list;
}

static void ht_walker(value k, value v, void *ctx) {
    value proc = (value) ctx;
    GET_CLOSURE(proc)->func(GET_CLOSURE(proc)->freevars, NO_CALL_FLAGS, 2, k, v);
}

value primcall_hash_table_walk(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("hash-table-walk needs two arguments"); }
    init_args();
    value ht = next_arg();
    value proc = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-walk first argument is not a hash-table"); }
    if (!IS_CLOSURE(proc)) { RAISE("hash-table-walk second argument is not a procedure"); }
    if (!CLOSURE_ACCEPTS(GET_CLOSURE(proc), 2)) {
        RAISE("hash-table-walk second argument (proc) procedure should accept two arguments");
    }

    hash_table_each(&GET_OBJECT(ht)->hash_table.ht, ht_walker, proc);

    return VOID;
}

struct folder_ctx {
    value func;
    value acc;
};

static void ht_folder(value k, value v, void *ctx) {
    struct folder_ctx *c = ctx;
    c->acc = GET_CLOSURE(c->func)->func(GET_CLOSURE(c->func)->freevars, NO_CALL_FLAGS, 3, k, v, c->acc);
}

value primcall_hash_table_fold(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { RAISE("hash-table-fold needs three arguments"); }
    init_args();
    value ht = next_arg();
    value f = next_arg();
    value init_value = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-fold first argument is not a hash-table"); }
    if (!IS_CLOSURE(f)) { RAISE("hash-table-fold second argument is not a procedure"); }
    if (!CLOSURE_ACCEPTS(GET_CLOSURE(f), 3)) {
        RAISE("hash-table-fold second argument (f) procedure should accept three arguments");
    }

    struct folder_ctx ctx = {
        .func = f,
        .acc = init_value,
    };
    hash_table_each(&GET_OBJECT(ht)->hash_table.ht, ht_folder, &ctx);

    return ctx.acc;
}

static void ht_to_alist(value k, value v, void *ctx) {
    value *alist = ctx;
    *alist = make_pair(make_pair(k, v), *alist);
}

value primcall_hash_table_to_alist(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table->alist needs a single argument"); }
    init_args();
    value ht = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table->alist argument is not a hash-table"); }

    value alist = NIL;
    hash_table_each(&GET_OBJECT(ht)->hash_table.ht, ht_to_alist, &alist);

    return alist;
}

static void ht_copier(value k, value v, void *ctx) {
    value owner = (value) ctx;
    struct hash_table *ht = &GET_OBJECT(owner)->hash_table.ht;
    hash_table_set(ht, owner, k, v);
}

value primcall_hash_table_copy(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table-copy needs a single argument"); }
    init_args();
    value ht = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-copy argument is not a hash-table"); }

    struct object *new_ht = alloc_object();
    new_ht->type = OBJ_HASH_TABLE;
    new_ht->hash_table.ht.user_hash_fn = GET_OBJECT(ht)->hash_table.ht.user_hash_fn;
    new_ht->hash_table.ht.user_eq_fn = GET_OBJECT(ht)->hash_table.ht.user_eq_fn;
    hash_table_init(&new_ht->hash_table.ht,
                    GET_OBJECT(ht)->hash_table.ht.cap,
                    GET_OBJECT(ht)->hash_table.ht.hash_fn,
                    GET_OBJECT(ht)->hash_table.ht.eq_fn);

    hash_table_each(&GET_OBJECT(ht)->hash_table.ht, ht_copier, OBJECT(new_ht));

    return OBJECT(new_ht);
}

value primcall_hash_table_merge_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("hash-table-merge! needs two arguments"); }
    init_args();
    value ht1 = next_arg();
    value ht2 = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht1)) { RAISE("hash-table-merge! first argument is not a hash-table"); }
    if (!IS_HASH_TABLE(ht2)) { RAISE("hash-table-merge! second argument is not a hash-table"); }

    hash_table_each(&GET_OBJECT(ht2)->hash_table.ht, ht_copier, ht1);

    return ht1;
}

value primcall_hash_by_identity(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("hash-by-identity needs one or two arguments"); }
    init_args();
    value v = next_arg();
    value bound = nargs == 2 ? next_arg() : FALSE;
    free_args();

    if (bound != FALSE && (!IS_FIXNUM(bound) || GET_FIXNUM(bound) <= 0)) {
        RAISE("hash-by-identity second argument must be a positive integer");
    }

    uint64_t h = hash_table_default_hash(NULL, v);

    /* we have only 61-bit signed integers right now, so shift by 4 bits
     * to get a positive integer in the appropriate range. */
    h >>= 4;

    if (bound != FALSE) {
        h %= GET_FIXNUM(bound);
    }

    return FIXNUM(h);
}

static uint64_t hash_string_djb2(const char *s, size_t len) {
    uint64_t h = 5381;
    for (size_t i = 0; i < len; i++) {
        h = ((h << 5) + h) ^ (unsigned char) s[i];
    }
    return h;
}

static uint64_t hash_string_ci_djb2(const char *s, size_t len) {
    uint64_t h = 5381;
    for (size_t i = 0; i < len; i++) {
        h = ((h << 5) + h) ^ (unsigned char) tolower(s[i]);
    }
    return h;
}

static uint64_t hash_equal_recursive(value v, int depth);

static uint64_t hash_vector_recursive(value *data, int len, int depth) {
    uint64_t h = 5381;
    for (int i = 0; i < len && i < 16; i++) {
        /* 2654435761 = 2^32/phi (Knuth multiplicative hash), good for avalanche mixing */
        h ^= hash_equal_recursive(data[i], depth - 1) * (2654435761ULL + i);
    }
    return h;
}

/* depth=8 covers typical nesting depth without risking stack overflow */
static uint64_t hash_equal_recursive(value v, int depth) {
    if (IS_STRING(v)) {
        struct string *s = GET_STRING(v);
        return hash_string_djb2(s->s, s->len);
    }
    if (depth > 0 && IS_PAIR(v)) {
        uint64_t h = hash_equal_recursive(GET_PAIR(v)->car, depth - 1);
        h ^= hash_equal_recursive(GET_PAIR(v)->cdr, depth - 1) * 2654435761ULL; /* Knuth */
        return h;
    }
    if (depth > 0 && IS_OBJECT(v) && GET_OBJECT(v)->type == OBJ_VECTOR) {
        return hash_vector_recursive(GET_OBJECT(v)->vector.data,
                                     GET_OBJECT(v)->vector.len, depth);
    }
    return hash_table_default_hash(NULL, v);
}

/* shift by 4 to land in the positive fixnum range (fixnums are 61-bit signed) */
static uint64_t hash_finalize(uint64_t h, value bound) {
    h >>= 4;
    if (bound != FALSE) {
        h %= GET_FIXNUM(bound);
    }
    return h;
}

value primcall_hash(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("hash needs one or two arguments"); }
    init_args();
    value v = next_arg();
    value bound = nargs == 2 ? next_arg() : FALSE;
    free_args();

    if (bound != FALSE && (!IS_FIXNUM(bound) || GET_FIXNUM(bound) <= 0)) {
        RAISE("hash second argument must be a positive integer");
    }

    return FIXNUM(hash_finalize(hash_equal_recursive(v, 8), bound));
}

value primcall_string_hash(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("string-hash needs one or two arguments"); }
    init_args();
    value v = next_arg();
    value bound = nargs == 2 ? next_arg() : FALSE;
    free_args();

    if (!IS_STRING(v)) { RAISE("string-hash first argument must be a string"); }
    if (bound != FALSE && (!IS_FIXNUM(bound) || GET_FIXNUM(bound) <= 0)) {
        RAISE("string-hash second argument must be a positive integer");
    }

    struct string *s = GET_STRING(v);
    return FIXNUM(hash_finalize(hash_string_djb2(s->s, s->len), bound));
}

value primcall_string_ci_hash(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1 && nargs != 2) { RAISE("string-ci-hash needs one or two arguments"); }
    init_args();
    value v = next_arg();
    value bound = nargs == 2 ? next_arg() : FALSE;
    free_args();

    if (!IS_STRING(v)) { RAISE("string-ci-hash first argument must be a string"); }
    if (bound != FALSE && (!IS_FIXNUM(bound) || GET_FIXNUM(bound) <= 0)) {
        RAISE("string-ci-hash second argument must be a positive integer");
    }

    struct string *s = GET_STRING(v);
    return FIXNUM(hash_finalize(hash_string_ci_djb2(s->s, s->len), bound));
}

 value primcall_hash_table_equivalence_function(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table-equivalence-function needs a single argument"); }
    init_args();
    value ht = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-equivalence-function argument is not a hash-table"); }
    return GET_OBJECT(ht)->hash_table.ht.user_eq_fn;
}

value primcall_hash_table_hash_function(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("hash-table-hash-function needs a single argument"); }
    init_args();
    value ht = next_arg();
    free_args();

    if (!IS_HASH_TABLE(ht)) { RAISE("hash-table-hash-function argument is not a hash-table"); }
    return GET_OBJECT(ht)->hash_table.ht.user_hash_fn;
}

value primcall_make_empty_environment(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("make-empty-environment takes no arguments"); }
    struct object *obj = alloc_object();
    obj->type = OBJ_ENVIRONMENT;
    obj->environment.hash_table = malloc(sizeof(struct hash_table));
    hash_table_init(obj->environment.hash_table, 8, NULL, NULL);
    return OBJECT(obj);
}

/* maps a sym_kind to the Scheme symbol that represents it in the
 * environment-lookup / environment-bind! contract. sym_unbound has no
 * symbol: absence is #f from environment-lookup, and environment-bind!
 * never writes it. */
static value sym_kind_to_symbol(enum sym_kind kind) {
    /* extend_global_env with sym_unbound is just interning here */
    switch (kind) {
    case sym_value:    return extend_global_env("value", 5, sym_unbound);
    case sym_macro:    return extend_global_env("macro", 5, sym_unbound);
    case sym_special:  return extend_global_env("special", 7, sym_unbound);
    case sym_aux:      return extend_global_env("aux", 3, sym_unbound);
    case sym_primcall: return extend_global_env("primcall", 8, sym_unbound);
    case sym_alias:    return extend_global_env("alias", 5, sym_unbound);
    default:
        RAISE("internal error: unhandled sym_kind case");
    }
}

/* the reverse of sym_kind_to_symbol */
static enum sym_kind symbol_to_sym_kind(value sym) {
    if (sym == extend_global_env("value", 5, sym_unbound))    return sym_value;
    if (sym == extend_global_env("macro", 5, sym_unbound))    return sym_macro;
    if (sym == extend_global_env("special", 7, sym_unbound))  return sym_special;
    if (sym == extend_global_env("aux", 3, sym_unbound))      return sym_aux;
    if (sym == extend_global_env("primcall", 8, sym_unbound)) return sym_primcall;
    if (sym == extend_global_env("alias", 5, sym_unbound))    return sym_alias;
    struct symbol *s = GET_SYMBOL(sym);
    RAISE("environment-bind!: invalid kind: %.*s", (int) s->name_len, s->name);
}

value primcall_environment_lookup(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("environment-lookup needs 2 arguments"); }
    init_args();
    value e = next_arg();
    value sym = next_arg();
    free_args();

    if (!IS_ENVIRONMENT(e)) { RAISE("environment-lookup first argument is not an environment"); }
    if (!IS_SYMBOL(sym)) { RAISE("environment-lookup second argument is not a symbol"); }

    struct hash_table *ht = GET_OBJECT(e)->environment.hash_table;
    if (ht == NULL) { RAISE("environment-lookup does not support the global environment"); }

    struct binding *binding = (struct binding *) hash_table_get(ht, 0, sym);
    if (binding == SENTINEL || binding->kind == sym_unbound) { return FALSE; }

    return make_pair(sym_kind_to_symbol(binding->kind), binding->value);
}

value primcall_environment_bind_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 4) { RAISE("environment-bind! needs 4 arguments"); }
    init_args();
    value e = next_arg();
    value sym = next_arg();
    value kind = next_arg();
    value val = next_arg();
    free_args();
    if (!IS_ENVIRONMENT(e)) { RAISE("environment-bind! first argument is not an environment"); }
    if (!IS_SYMBOL(sym)) { RAISE("environment-bind! second argument is not a symbol"); }
    if (!IS_SYMBOL(kind)) { RAISE("environment-bind! third argument is not a symbol"); }

    struct hash_table *ht = GET_OBJECT(e)->environment.hash_table;
    if (ht == NULL) { RAISE("environment-bind! does not support the sentinel environment"); }

    env_define(e, sym, val, symbol_to_sym_kind(kind));
    return VOID;
}

value primcall_environment_q(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("environment? needs a single argument"); }
    init_args();
    value v = next_arg();
    free_args();
    return BOOL(IS_ENVIRONMENT(v));
}

value primcall_run_so(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { RAISE("run-so needs two arguments"); }
    init_args();
    value filename = next_arg();
    value e = next_arg();
    free_args();

    if (!IS_STRING(filename)) { RAISE("run-so first argument (filename) must be a string"); }
    if (!IS_ENVIRONMENT(e)) { RAISE("run-so second argument (environment) must be an environment"); }

    char *filenamez = strz(filename);
    void *handle = dlopen(filenamez, RTLD_NOW | RTLD_LOCAL);
    free(filenamez);
    if (!handle) { RAISE("run-so: cannot load file: %.*s", (int) GET_STRING(filename)->len, GET_STRING(filename)->s); }

    value (*whisper_main_sym)(value env) = dlsym(handle, "whisper_main");
    if (!whisper_main_sym) { RAISE("run-so: cannot find symbol whisper_main in shared object"); }

    value result = whisper_main_sym(e);

    /* leave the handle open since otherwise we won't have access to
     * functionality inside it afterwards */

    return result;
}

value primcall_list_directory(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("list-directory needs a single argument"); }
    init_args();
    value path = next_arg();
    free_args();

    if (!IS_STRING(path)) { RAISE("list-directory argument is not a string"); }

    char *pathz = strz(path);
    DIR *dir = opendir(pathz);
    free(pathz);
    if (!dir) { return FALSE; }

    value entries = NIL;
    struct dirent *ent;
    while ((ent = readdir(dir)) != NULL) {
        if (strcmp(ent->d_name, ".") == 0 || strcmp(ent->d_name, "..") == 0) { continue; }
        value s = make_string(ent->d_name, strlen(ent->d_name));
        entries = make_pair(s, entries);
    }

    closedir(dir);

    return reverse_list(entries, NIL);
}

value primcall_gc(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("gc takes no arguments"); }
    gc();
    return VOID;
}

value primcall_gc_manual_mode_b(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { RAISE("gc-manual-mode! needs a single argument"); }
    init_args();
    value enabled = next_arg();
    free_args();
    gc_manual_mode = (enabled != FALSE);
    return VOID;
}

value primcall_percent_gc_stats(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { RAISE("%%gc-stats takes no arguments"); }

    /* snapshot counters before allocating */
    size_t full_sweeps = gc_full_sweeps;
    size_t lazy_reclaims = gc_lazy_reclaims;
    size_t live = gc_marked_count;
    uint64_t epoch = gc_epoch;
    int manual = gc_manual_mode;

    size_t pools = 0;
    for (int i = 0; i < n_heaps; ++i) {
        for (struct pool *p = heaps[i]; p; p = p->next) {
            ++pools;
        }
    }

    value result = make_vector(6, VOID);
    GET_OBJECT(result)->vector.data[0] = FIXNUM(epoch);
    GET_OBJECT(result)->vector.data[1] = FIXNUM(full_sweeps);
    GET_OBJECT(result)->vector.data[2] = FIXNUM(lazy_reclaims);
    GET_OBJECT(result)->vector.data[3] = FIXNUM(live);
    GET_OBJECT(result)->vector.data[4] = FIXNUM(gc_manual_mode);
    GET_OBJECT(result)->vector.data[5] = FIXNUM(pools);

    return result;
}
