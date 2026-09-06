#include "../core.h"

#include <raylib.h>

Color color_from_list(value color_scm, const char *caller_name) {
    const char *error_msg = "color must be a list of four values (r g b a), each from in range [0, 255]";
    if (!IS_PAIR(color_scm)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *first_pair = GET_PAIR(color_scm);
    if (!IS_PAIR(first_pair->cdr)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *second_pair = GET_PAIR(first_pair->cdr);
    if (!IS_PAIR(second_pair->cdr)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *third_pair = GET_PAIR(second_pair->cdr);
    if (!IS_PAIR(third_pair->cdr)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *fourth_pair = GET_PAIR(third_pair->cdr);

    if (!IS_FIXNUM(first_pair->car)) { raise_error("%s: %s", caller_name, error_msg); }
    if (!IS_FIXNUM(second_pair->car)) { raise_error("%s: %s", caller_name, error_msg); }
    if (!IS_FIXNUM(third_pair->car)) { raise_error("%s: %s", caller_name, error_msg); }
    if (!IS_FIXNUM(fourth_pair->car)) { raise_error("%s: %s", caller_name, error_msg); }

    int r = GET_FIXNUM(first_pair->car);
    int g = GET_FIXNUM(second_pair->car);
    int b = GET_FIXNUM(third_pair->car);
    int a = GET_FIXNUM(fourth_pair->car);

    if (r < 0 || r > 255) { raise_error("%s: %s", caller_name, error_msg); }
    if (g < 0 || g > 255) { raise_error("%s: %s", caller_name, error_msg); }
    if (b < 0 || b > 255) { raise_error("%s: %s", caller_name, error_msg); }
    if (a < 0 || a > 255) { raise_error("%s: %s", caller_name, error_msg); }

    Color color;
    color.r = r;
    color.g = g;
    color.b = b;
    color.a = a;

    return color;
}

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

value close_window(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("close-window accepts no arguments"); }
    CloseWindow();
    return VOID;
}

value window_should_close(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("window-should-close accepts no arguments"); }
    return BOOL(WindowShouldClose());
}

value clear_background(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("clear-background takes a single argument"); }

    init_args();
    value color_scm = next_arg();
    free_args();

    Color color = color_from_list(color_scm, "clear-background");

    ClearBackground(color);
    return VOID;
}

value begin_drawing(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("begin-drawing accepts no arguments"); }
    BeginDrawing();
    return VOID;
}

value end_drawing(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("end-drawing accepts no arguments"); }
    EndDrawing();
    return VOID;
}
