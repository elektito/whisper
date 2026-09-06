#include "../core.h"

#include <raylib.h>

value init_window(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { raise_error("init-window needs three arguments"); }
    init_args();
    value width = next_arg();
    value height = next_arg();
    value title = next_arg();
    free_args();

    if (!IS_FIXNUM(width)) { raise_error("init-window first argument (width) must be an integer"); }
    if (!IS_FIXNUM(height)) { raise_error("init-window second argument (height) must be an integer"); }
    if (!IS_STRING(title)) { raise_error("init-window third argument (title) must be a string"); }

    char title_c[GET_STRING(title)->len + 1];
    memcpy(title_c, GET_STRING(title)->s, GET_STRING(title)->len);
    title_c[GET_STRING(title)->len] = 0;

    InitWindow(GET_FIXNUM(width), GET_FIXNUM(height), title_c);

    return VOID;
}
