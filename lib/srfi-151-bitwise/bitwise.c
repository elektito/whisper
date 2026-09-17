#include "../core.h"

value bitwise_not(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("bitwise-not needs a single argument"); }

    init_args();
    value i = next_arg();
    free_args();

    if (!IS_FIXNUM(i)) { raise_error("bitwise-not argument is not an integer"); }

    return FIXNUM(~GET_FIXNUM(i));
}

value bitwise_and(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs == 0) { return FIXNUM(-1); }

    int64_t result = -1;
    init_args();
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        if (!IS_FIXNUM(v)) {
            free_args();
            raise_error("bitwise-and arguments must all be integers");
        }
        result &= GET_FIXNUM(v);
    }
    free_args();

    return FIXNUM(result);
}

value bitwise_ior(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs == 0) { return FIXNUM(0); }

    int64_t result = 0;
    init_args();
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        if (!IS_FIXNUM(v)) {
            free_args();
            raise_error("bitwise-ior arguments must all be integers");
        }
        result |= GET_FIXNUM(v);
    }
    free_args();

    return FIXNUM(result);
}

value bitwise_xor(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs == 0) { return FIXNUM(0); }

    int64_t result = 0;
    init_args();
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        if (!IS_FIXNUM(v)) {
            free_args();
            raise_error("bitwise-xor arguments must all be integers");
        }
        result ^= GET_FIXNUM(v);
    }
    free_args();

    return FIXNUM(result);
}

value bitwise_eqv(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs == 0) { return FIXNUM(-1); }

    int64_t result = -1;
    init_args();
    for (int i = 0; i < nargs; ++i) {
        value v = next_arg();
        if (!IS_FIXNUM(v)) {
            free_args();
            raise_error("bitwise-eqv arguments must all be integers");
        }
        result = ~(result ^ GET_FIXNUM(v));
    }
    free_args();

    return FIXNUM(result);
}

value integer_length(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("integer-length needs a single argument"); }

    init_args();
    value v = next_arg();
    free_args();

    if (!IS_FIXNUM(v)) { raise_error("integer-length argument must be an integer"); }

    int64_t i = GET_FIXNUM(v);

    /* for negative numbers, integer-length(i) == integer-length(~i) */
    if (i < 0) {
        i = ~i;
    }

    /* __builtin_clzll(0) is undefined behavior, so handle 0 explicitly */
    if (i == 0) {
        return FIXNUM(0);
    }

    /* 64 minus leading zeros gives the position of the highest set bit + 1 */
    int length = 64 - __builtin_clzll((uint64_t) i);
    return FIXNUM(length);
}

value bit_count(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("bit-count needs a single argument"); }

    init_args();
    value v = next_arg();
    free_args();

    if (!IS_FIXNUM(v)) { raise_error("bit-count argument must be an integer"); }

    int64_t i = GET_FIXNUM(v);

    /* for negative numbers we should return the population count of zeros */
    if (i < 0) {
        i = ~i;
    }

    int count = __builtin_popcountll((uint64_t)i);
    return FIXNUM(count);
}

value arithmetic_shift(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { raise_error("arithmetic-shift needs two arguments"); }

    init_args();
    value n = next_arg();
    value count = next_arg();
    free_args();

    if (!IS_FIXNUM(n)) { raise_error("arithmetic-shift first argument must be an integer"); }
    if (!IS_FIXNUM(count)) { raise_error("arithmetic-shift second argument must be an integer"); }

    int64_t i = GET_FIXNUM(n);
    int64_t c = GET_FIXNUM(count);

    if (c == 0) {
        return FIXNUM(i);
    }

    if (c < 0) {
        /* negative count = arithmetic right shift */
        int64_t shift = -c;
        if (shift >= 61) {
            /* shifting right past 61 bits leaves only sign bits: -1 for
             * negative, 0 for positive */
            return FIXNUM(i < 0 ? -1 : 0);
        }
        return FIXNUM(i >> shift); // int64_t performs arithmetic right shift in C
    } else {
        /* positive count = left shift */
        if (c >= 61) {
            /* shifting left past 61 bits truncates all original payload bits to zero */
            return FIXNUM(0);
        }
        return FIXNUM((int64_t)((uint64_t)i << c));
    }
}
