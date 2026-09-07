#include "../core.h"

#include <raylib.h>

#include <time.h>

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

value set_target_fps(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("set-target-fps takes a single argument"); }

    init_args();
    value fps = next_arg();
    free_args();

    if (!IS_FIXNUM(fps)) { raise_error("set-target-fps argument is not an integer"); }

    SetTargetFPS(GET_FIXNUM(fps));

    return VOID;
}

value get_frame_time(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-frame-time accepts no arguments"); }
    return FLONUM(GetFrameTime());
}

value get_time_us(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-time-us accepts no arguments"); }

    struct timespec ts;
    int ret = clock_gettime(CLOCK_MONOTONIC, &ts);
    if (ret) { raise_error("error reading time"); }

    uint64_t usecs = ts.tv_sec * 1000000 + ts.tv_nsec * 1000;
    return FIXNUM(usecs);
}

value get_fps(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-fps accepts no arguments"); }
    return FIXNUM(GetFPS());
}

value is_key_pressed(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-key-pressed takes a single argument"); }

    init_args();
    value key = next_arg();
    free_args();

    if (!IS_FIXNUM(key)) { raise_error("is-key-pressed argument is not an integer"); }

    int result = IsKeyPressed(GET_FIXNUM(key));
    return BOOL(result);
}

value is_key_pressed_repeat(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-key-pressed-repeat takes a single argument"); }

    init_args();
    value key = next_arg();
    free_args();

    if (!IS_FIXNUM(key)) { raise_error("is-key-pressed-repeat argument is not an integer"); }

    int result = IsKeyPressedRepeat(GET_FIXNUM(key));
    return BOOL(result);
}

value is_key_down(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-key-down takes a single argument"); }

    init_args();
    value key = next_arg();
    free_args();

    if (!IS_FIXNUM(key)) { raise_error("is-key-down argument is not an integer"); }

    int result = IsKeyDown(GET_FIXNUM(key));
    return BOOL(result);
}

value is_key_released(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-key-released takes a single argument"); }

    init_args();
    value key = next_arg();
    free_args();

    if (!IS_FIXNUM(key)) { raise_error("is-key-released argument is not an integer"); }

    int result = IsKeyReleased(GET_FIXNUM(key));
    return BOOL(result);
}

value is_key_up(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-key-up takes a single argument"); }

    init_args();
    value key = next_arg();
    free_args();

    if (!IS_FIXNUM(key)) { raise_error("is-key-up argument is not an integer"); }

    int result = IsKeyUp(GET_FIXNUM(key));
    return BOOL(result);
}

value draw_pixel(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 3) { raise_error("draw-pixel takes three arguments"); }

    init_args();
    value x = next_arg();
    value y = next_arg();
    value color_scm = next_arg();
    free_args();

    if (!IS_FIXNUM(x)) { raise_error("draw-pixel first argument (x) is not an integer"); }
    if (!IS_FIXNUM(y)) { raise_error("draw-pixel second argument (y) is not an integer"); }

    Color color = color_from_list(color_scm, "draw-pixel");
    DrawPixel(GET_FIXNUM(x), GET_FIXNUM(y), color);

    return VOID;
}

value draw_line(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 5) { raise_error("draw-line takes five arguments"); }

    init_args();
    value start_x = next_arg();
    value start_y = next_arg();
    value end_x = next_arg();
    value end_y = next_arg();
    value color_scm = next_arg();
    free_args();

    if (!IS_FIXNUM(start_x)) { raise_error("draw-line first argument (start-x) is not an integer"); }
    if (!IS_FIXNUM(start_y)) { raise_error("draw-line second argument (start-y) is not an integer"); }
    if (!IS_FIXNUM(end_x)) { raise_error("draw-line third argument (end-x) is not an integer"); }
    if (!IS_FIXNUM(end_y)) { raise_error("draw-line fourth argument (end-y) is not an integer"); }

    Color color = color_from_list(color_scm, "draw-line");
    DrawLine(GET_FIXNUM(start_x), GET_FIXNUM(start_y), GET_FIXNUM(end_x), GET_FIXNUM(end_y), color);

    return VOID;
}

value draw_circle(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 4) { raise_error("draw-circle takes four arguments"); }

    init_args();
    value x = next_arg();
    value y = next_arg();
    value r = next_arg();
    value color_scm = next_arg();
    free_args();

    if (!IS_FIXNUM(x)) { raise_error("draw-circle first argument (x) is not an integer"); }
    if (!IS_FIXNUM(y)) { raise_error("draw-circle second argument (y) is not an integer"); }
    if (!IS_FIXNUM(r)) { raise_error("draw-circle third argument (radius) is not an integer"); }

    Color color = color_from_list(color_scm, "draw-circle");
    DrawCircle(GET_FIXNUM(x), GET_FIXNUM(y), GET_FIXNUM(r), color);

    return VOID;
}

value draw_rectangle(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 5) { raise_error("draw-rectangle takes five arguments"); }

    init_args();
    value x = next_arg();
    value y = next_arg();
    value w = next_arg();
    value h = next_arg();
    value color_scm = next_arg();
    free_args();

    if (!IS_FIXNUM(x)) { raise_error("draw-rectangle first argument (x) is not an integer"); }
    if (!IS_FIXNUM(y)) { raise_error("draw-rectangle second argument (y) is not an integer"); }
    if (!IS_FIXNUM(w)) { raise_error("draw-rectangle third argument (width) is not an integer"); }
    if (!IS_FIXNUM(h)) { raise_error("draw-rectangle fourth argument (height) is not an integer"); }

    Color color = color_from_list(color_scm, "draw-line");
    DrawRectangle(GET_FIXNUM(x), GET_FIXNUM(y), GET_FIXNUM(w), GET_FIXNUM(h), color);

    return VOID;
}

value draw_fps(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { raise_error("draw-fps takes two arguments"); }

    init_args();
    value x = next_arg();
    value y = next_arg();
    free_args();

    if (!IS_FIXNUM(x)) { raise_error("draw-fps first argument (x) is not an integer"); }
    if (!IS_FIXNUM(y)) { raise_error("draw-fps second argument (y) is not an integer"); }

    DrawFPS(GET_FIXNUM(x), GET_FIXNUM(y));

    return VOID;
}

value draw_text(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 5) { raise_error("draw-text takes five arguments"); }

    init_args();
    value text_scm = next_arg();
    value x = next_arg();
    value y = next_arg();
    value font_size = next_arg();
    value color_scm = next_arg();
    free_args();

    if (!IS_STRING(text_scm)) { raise_error("draw-text first argument (text) is not a string"); }
    if (!IS_FIXNUM(x)) { raise_error("draw-text second argument (x) is not an integer"); }
    if (!IS_FIXNUM(y)) { raise_error("draw-text third argument (y) is not an integer"); }
    if (!IS_FIXNUM(font_size)) { raise_error("draw-text fourth argument (text-size) is not an integer"); }

    char text[GET_STRING(text_scm)->len + 1];
    memcpy(text, GET_STRING(text_scm)->s, GET_STRING(text_scm)->len);
    text[GET_STRING(text_scm)->len] = 0;
    Color color = color_from_list(color_scm, "draw-text");
    DrawText(text, GET_FIXNUM(x), GET_FIXNUM(y), GET_FIXNUM(font_size), color);

    return VOID;
}
