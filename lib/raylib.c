#include "../core.h"

#include <raylib.h>

#include <time.h>

static int wrapped_kind_texture2d;
static int wrapped_kind_rendertexture2d;

__attribute__((constructor))
static void init_lib(void) {
    wrapped_kind_texture2d = assign_c_wrapped_kind();
    wrapped_kind_rendertexture2d = assign_c_wrapped_kind();
}

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

Rectangle rect_from_list(value rect_scm, const char *caller_name) {
    const char *error_msg = "rectangle must be a list of four values (x y w h), each a flonum";
    if (!IS_PAIR(rect_scm)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *first_pair = GET_PAIR(rect_scm);
    if (!IS_PAIR(first_pair->cdr)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *second_pair = GET_PAIR(first_pair->cdr);
    if (!IS_PAIR(second_pair->cdr)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *third_pair = GET_PAIR(second_pair->cdr);
    if (!IS_PAIR(third_pair->cdr)) { raise_error("%s: %s", caller_name, error_msg); }
    struct pair *fourth_pair = GET_PAIR(third_pair->cdr);

    float x, y, w, h;

    if (IS_FIXNUM(first_pair->car)) {
        x = (float) GET_FIXNUM(first_pair->car);
    } else if (IS_FLONUM(first_pair->car)) {
        x = GET_FLONUM(first_pair->car);
    } else {
        raise_error("%s: %s", caller_name, error_msg);
    }

    if (IS_FIXNUM(second_pair->car)) {
        y = (float) GET_FIXNUM(second_pair->car);
    } else if (IS_FLONUM(second_pair->car)) {
        y = GET_FLONUM(second_pair->car);
    } else {
        raise_error("%s: %s", caller_name, error_msg);
    }

    if (IS_FIXNUM(third_pair->car)) {
        w = (float) GET_FIXNUM(third_pair->car);
    } else if (IS_FLONUM(third_pair->car)) {
        w = GET_FLONUM(third_pair->car);
    } else {
        raise_error("%s: %s", caller_name, error_msg);
    }

    if (IS_FIXNUM(fourth_pair->car)) {
        h = (float) GET_FIXNUM(fourth_pair->car);
    } else if (IS_FLONUM(fourth_pair->car)) {
        h = GET_FLONUM(fourth_pair->car);
    } else {
        raise_error("%s: %s", caller_name, error_msg);
    }

    Rectangle rect;
    rect.x = x;
    rect.y = y;
    rect.width = w;
    rect.height = h;

    return rect;
}

Vector2 vector2_from_pair(value pair, const char *caller_name, const char *value_name) {
    if (!IS_PAIR(pair)) { raise_error("%s: %s must be a pair of floats", caller_name, value_name); }

    Vector2 vec;

    if (IS_FIXNUM(GET_PAIR(pair)->car)) {
        vec.x = (float) GET_FIXNUM(GET_PAIR(pair)->car);
    } else if (IS_FIXNUM(GET_PAIR(pair)->car)) {
        vec.x = GET_FLONUM(GET_PAIR(pair)->car);
    } else {
        raise_error("%s: %s must be a pair of floats", caller_name, value_name);
    }

    if (IS_FIXNUM(GET_PAIR(pair)->cdr)) {
        vec.y = (float) GET_FIXNUM(GET_PAIR(pair)->cdr);
    } else if (IS_FIXNUM(GET_PAIR(pair)->cdr)) {
        vec.y = GET_FLONUM(GET_PAIR(pair)->cdr);
    } else {
        raise_error("%s: %s must be a pair of floats", caller_name, value_name);
    }

    return vec;
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

value is_window_ready(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("is-window-ready accepts no arguments"); }
    return BOOL(IsWindowReady());
}

value is_window_fullscreen(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("is-window-fullscreen accepts no arguments"); }
    return BOOL(IsWindowFullscreen());
}

value is_window_hidden(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("is-window-hidden accepts no arguments"); }
    return BOOL(IsWindowHidden());
}

value is_window_minimized(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("is-window-minimized accepts no arguments"); }
    return BOOL(IsWindowMinimized());
}

value is_window_maximized(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("is-window-maximized accepts no arguments"); }
    return BOOL(IsWindowMaximized());
}

value is_window_focused(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("is-window-focused accepts no arguments"); }
    return BOOL(IsWindowFocused());
}

value is_window_resized(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("is-window-resized accepts no arguments"); }
    return BOOL(IsWindowResized());
}

value is_window_state(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-window-state takes a single argument"); }

    init_args();
    value flag = next_arg();
    free_args();

    if (!IS_FIXNUM(flag)) { raise_error("is-window-state argument is not an integer"); }

    return BOOL(IsWindowState(GET_FIXNUM(flag)));
}

value set_window_state(environment env, enum call_flags flags, int nargs, ...) {
    unsigned int fs = 0;

    init_args();
    for (int i = 0; i < nargs; ++i) {
        value f = next_arg();
        if (!IS_FIXNUM(f)) { raise_error("set-window-state arguments must be integers"); }
        fs |= GET_FIXNUM(f);
    }
    free_args();

    SetWindowState(fs);

    return VOID;
}

value clear_window_state(environment env, enum call_flags flags, int nargs, ...) {
    unsigned int fs = 0;

    init_args();
    for (int i = 0; i < nargs; ++i) {
        value f = next_arg();
        if (!IS_FIXNUM(f)) { raise_error("clear-window-state arguments must be integers"); }
        fs |= GET_FIXNUM(f);
    }
    free_args();

    ClearWindowState(fs);

    return VOID;
}

value toggle_fullscreen(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("toggle-fullscreen accepts no arguments"); }
    ToggleFullscreen();
    return VOID;
}

value toggle_borderless_windowed(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("toggle-borderless-windowed accepts no arguments"); }
    ToggleBorderlessWindowed();
    return VOID;
}

value maximize_window(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("maximize-window accepts no arguments"); }
    MaximizeWindow();
    return VOID;
}

value minimize_window(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("minimize-window accepts no arguments"); }
    MinimizeWindow();
    return VOID;
}

value restore_window(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("restore-window accepts no arguments"); }
    RestoreWindow();
    return VOID;
}

value set_window_title(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("set-window-title takes a single argument"); }

    init_args();
    value title_scm = next_arg();
    free_args();

    if (!IS_STRING(title_scm)) { raise_error("set-window-title argument is not a string"); }

    char title[GET_STRING(title_scm)->len + 1];
    memcpy(title, GET_STRING(title_scm)->s, GET_STRING(title_scm)->len);
    title[GET_STRING(title_scm)->len] = 0;

    SetWindowTitle(title);

    return VOID;
}

value set_window_position(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { raise_error("set-window-position takes two arguments"); }

    init_args();
    value x = next_arg();
    value y = next_arg();
    free_args();

    if (!IS_FIXNUM(x)) { raise_error("set-window-position first argument (x) is not an integer"); }
    if (!IS_FIXNUM(y)) { raise_error("set-window-position second argument (y) is not an integer"); }

    SetWindowPosition(GET_FIXNUM(x), GET_FIXNUM(y));

    return VOID;
}

value set_window_min_size(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { raise_error("set-window-min-size takes two arguments"); }

    init_args();
    value w = next_arg();
    value h = next_arg();
    free_args();

    if (!IS_FIXNUM(w)) { raise_error("set-window-min-size first argument (width) is not an integer"); }
    if (!IS_FIXNUM(h)) { raise_error("set-window-min-size second argument (height) is not an integer"); }

    SetWindowMinSize(GET_FIXNUM(w), GET_FIXNUM(h));

    return VOID;
}

value set_window_max_size(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { raise_error("set-window-max-size takes two arguments"); }

    init_args();
    value w = next_arg();
    value h = next_arg();
    free_args();

    if (!IS_FIXNUM(w)) { raise_error("set-window-max-size first argument (width) is not an integer"); }
    if (!IS_FIXNUM(h)) { raise_error("set-window-max-size second argument (height) is not an integer"); }

    SetWindowMaxSize(GET_FIXNUM(w), GET_FIXNUM(h));

    return VOID;
}

value set_window_size(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { raise_error("set-window-size takes two arguments"); }

    init_args();
    value w = next_arg();
    value h = next_arg();
    free_args();

    if (!IS_FIXNUM(w)) { raise_error("set-window-size first argument (width) is not an integer"); }
    if (!IS_FIXNUM(h)) { raise_error("set-window-size second argument (height) is not an integer"); }

    SetWindowSize(GET_FIXNUM(w), GET_FIXNUM(h));

    return VOID;
}

value get_screen_width(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-screen-width accepts no arguments"); }
    return FIXNUM(GetScreenWidth());
}

value get_screen_height(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-screen-height accepts no arguments"); }
    return FIXNUM(GetScreenHeight());
}

value get_render_width(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-render-width accepts no arguments"); }
    return FIXNUM(GetRenderWidth());
}

value get_render_height(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-render-height accepts no arguments"); }
    return FIXNUM(GetRenderHeight());
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

value begin_texture_mode(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("begin-texture-mode takes a single argument"); }

    init_args();
    value texture_scm = next_arg();
    free_args();

    if (!IS_OBJECT(texture_scm)) { raise_error("begin-texture-mode argument is not a render texture"); }
    struct object *texture_obj = GET_OBJECT(texture_scm);
    if (texture_obj->type != OBJ_C_WRAPPED || texture_obj->c_wrapped.kind != wrapped_kind_rendertexture2d) {
        raise_error("begin-texture-mode argument is not a render texture");
    }

    BeginTextureMode(*(RenderTexture2D*)texture_obj->c_wrapped.data);

    return VOID;
}

value end_texture_mode(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("end-texture-mode accepts no arguments"); }
    EndTextureMode();
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

    uint64_t usecs = (uint64_t) ts.tv_sec * 1000000ULL + ts.tv_nsec / 1000;
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

value is_mouse_button_pressed(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-mouse-button-pressed takes a single argument"); }

    init_args();
    value button = next_arg();
    free_args();

    if (!IS_FIXNUM(button)) { raise_error("is-mouse-button-pressed argument is not an integer"); }

    int result = IsMouseButtonPressed(GET_FIXNUM(button));
    return BOOL(result);
}

value is_mouse_button_down(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-mouse-button-down takes a single argument"); }

    init_args();
    value button = next_arg();
    free_args();

    if (!IS_FIXNUM(button)) { raise_error("is-mouse-button-down argument is not an integer"); }

    int result = IsMouseButtonDown(GET_FIXNUM(button));
    return BOOL(result);
}

value is_mouse_button_released(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-mouse-button-released takes a single argument"); }

    init_args();
    value button = next_arg();
    free_args();

    if (!IS_FIXNUM(button)) { raise_error("is-mouse-button-released argument is not an integer"); }

    int result = IsMouseButtonReleased(GET_FIXNUM(button));
    return BOOL(result);
}

value is_mouse_button_up(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-mouse-button-up takes a single argument"); }

    init_args();
    value button = next_arg();
    free_args();

    if (!IS_FIXNUM(button)) { raise_error("is-mouse-button-up argument is not an integer"); }

    int result = IsMouseButtonUp(GET_FIXNUM(button));
    return BOOL(result);
}

value get_mouse_x(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-mouse-x accepts no arguments"); }
    return FIXNUM(GetMouseX());
}

value get_mouse_y(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-mouse-y accepts no arguments"); }
    return FIXNUM(GetMouseY());
}

value get_mouse_position(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-mouse-position accepts no arguments"); }
    Vector2 pos = GetMousePosition();
    return make_pair(FLONUM(pos.x), FLONUM(pos.y));
}

value get_mouse_delta(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 0) { raise_error("get-mouse-delta accepts no arguments"); }
    Vector2 delta = GetMouseDelta();
    return make_pair(FLONUM(delta.x), FLONUM(delta.y));
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

value load_texture(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("load-texture takes a single argument"); }

    init_args();
    value filename_scm = next_arg();
    free_args();

    if (!IS_STRING(filename_scm)) { raise_error("load-texture argument is not a string"); }

    char filename[GET_STRING(filename_scm)->len + 1];
    memcpy(filename, GET_STRING(filename_scm)->s, GET_STRING(filename_scm)->len);
    filename[GET_STRING(filename_scm)->len] = 0;

    Texture2D *texture = malloc(sizeof(Texture2D));
    *texture = LoadTexture(filename);
    struct object *obj = alloc_object();
    obj->type = OBJ_C_WRAPPED;
    obj->c_wrapped.kind = wrapped_kind_texture2d;
    obj->c_wrapped.data = texture;
    obj->c_wrapped.free_data = free;

    return OBJECT(obj);
}

value load_render_texture(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 2) { raise_error("load-render-texture takes two arguments"); }

    init_args();
    value w = next_arg();
    value h = next_arg();
    free_args();

    if (!IS_FIXNUM(w)) { raise_error("load-render-texture first argument (width) is not an integer"); }
    if (!IS_FIXNUM(h)) { raise_error("load-render-texture second argument (height) is not an integer"); }

    RenderTexture2D *texture = malloc(sizeof(RenderTexture2D));
    *texture = LoadRenderTexture(GET_FIXNUM(w), GET_FIXNUM(h));
    struct object *obj = alloc_object();
    obj->type = OBJ_C_WRAPPED;
    obj->c_wrapped.kind = wrapped_kind_rendertexture2d;
    obj->c_wrapped.data = texture;
    obj->c_wrapped.free_data = free;

    return OBJECT(obj);
}

value is_texture_valid(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-texture-valid takes a single argument"); }

    init_args();
    value texture_scm = next_arg();
    free_args();

    if (!IS_OBJECT(texture_scm)) { raise_error("is-texture-valid argument is not a texture"); }
    struct object *texture_obj = GET_OBJECT(texture_scm);
    if (texture_obj->type != OBJ_C_WRAPPED || texture_obj->c_wrapped.kind != wrapped_kind_texture2d) {
        raise_error("is-texture-valid argument is not a texture");
    }

    return BOOL(IsTextureValid(*(Texture2D*)texture_obj->c_wrapped.data));
}

value is_render_texture_valid(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("is-render-texture-valid takes a single argument"); }

    init_args();
    value texture_scm = next_arg();
    free_args();

    if (!IS_OBJECT(texture_scm)) { raise_error("is-render-texture-valid argument is not a render texture"); }
    struct object *texture_obj = GET_OBJECT(texture_scm);
    if (texture_obj->type != OBJ_C_WRAPPED || texture_obj->c_wrapped.kind != wrapped_kind_rendertexture2d) {
        raise_error("is-render-texture-valid argument is not a render texture");
    }

    return BOOL(IsRenderTextureValid(*(RenderTexture2D*)texture_obj->c_wrapped.data));
}

value unload_texture(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("unload-texture takes a single argument"); }

    init_args();
    value texture_scm = next_arg();
    free_args();

    if (!IS_OBJECT(texture_scm)) { raise_error("unload-texture argument is not a texture"); }
    struct object *texture_obj = GET_OBJECT(texture_scm);
    if (texture_obj->type != OBJ_C_WRAPPED || texture_obj->c_wrapped.kind != wrapped_kind_texture2d) {
        raise_error("unload-texture argument is not a texture");
    }

    UnloadTexture(*(Texture2D*)texture_obj->c_wrapped.data);

    return VOID;
}

value unload_render_texture(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("unload-render-texture takes a single argument"); }

    init_args();
    value texture_scm = next_arg();
    free_args();

    if (!IS_OBJECT(texture_scm)) { raise_error("unload-render-texture argument is not a render texture"); }
    struct object *texture_obj = GET_OBJECT(texture_scm);
    if (texture_obj->type != OBJ_C_WRAPPED || texture_obj->c_wrapped.kind != wrapped_kind_rendertexture2d) {
        raise_error("unload-render-texture argument is not a render texture");
    }

    UnloadRenderTexture(*(RenderTexture2D*)texture_obj->c_wrapped.data);

    return VOID;
}

value get_render_texture_texture(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 1) { raise_error("get-render-texture-texture takes a single argument"); }

    init_args();
    value render_texture_scm = next_arg();
    free_args();

    if (!IS_OBJECT(render_texture_scm)) { raise_error("get-render-texture-texture argument is not a render texture"); }
    struct object *render_texture_obj = GET_OBJECT(render_texture_scm);
    if (render_texture_obj->type != OBJ_C_WRAPPED || render_texture_obj->c_wrapped.kind != wrapped_kind_rendertexture2d) {
        raise_error("get-render-texture-texture argument is not a render texture");
    }

    Texture2D *texture = &((RenderTexture2D*) render_texture_obj->c_wrapped.data)->texture;
    struct object *obj = alloc_object();
    obj->type = OBJ_C_WRAPPED;
    obj->c_wrapped.kind = wrapped_kind_texture2d;
    obj->c_wrapped.data = texture;
    obj->c_wrapped.free_data = NULL;

    return OBJECT(obj);
}

value draw_texture(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 4) { raise_error("draw-texture takes four arguments"); }

    init_args();
    value texture_scm = next_arg();
    value x = next_arg();
    value y = next_arg();
    value tint_scm = next_arg();
    free_args();

    if (!IS_OBJECT(texture_scm)) { raise_error("draw-texture first argument is not a texture"); }
    struct object *texture_obj = GET_OBJECT(texture_scm);
    if (texture_obj->type != OBJ_C_WRAPPED || texture_obj->c_wrapped.kind != wrapped_kind_texture2d) {
        raise_error("draw-texture first argument is not a texture");
    }

    if (!IS_FIXNUM(x)) { raise_error("draw-texture second argument (x) is not an integer"); }
    if (!IS_FIXNUM(y)) { raise_error("draw-texture third argument (y) is not an integer"); }

    Color tint = color_from_list(tint_scm, "draw-texture");
    DrawTexture(*(Texture2D*)texture_obj->c_wrapped.data, GET_FIXNUM(x), GET_FIXNUM(y), tint);

    return VOID;
}

value draw_texture_pro(environment env, enum call_flags flags, int nargs, ...) {
    if (nargs != 6) { raise_error("draw-texture-pro takes six arguments"); }

    init_args();
    value texture_scm = next_arg();
    value source_rect_scm = next_arg();
    value dest_rect_scm = next_arg();
    value origin_scm = next_arg();
    value rotation = next_arg();
    value tint_scm = next_arg();
    free_args();

    if (!IS_OBJECT(texture_scm)) { raise_error("draw-texture-pro first argument is not a texture"); }
    struct object *texture_obj = GET_OBJECT(texture_scm);
    if (texture_obj->type != OBJ_C_WRAPPED || texture_obj->c_wrapped.kind != wrapped_kind_texture2d) {
        raise_error("draw-texture-pro first argument is not a texture");
    }

    if (!IS_FLONUM(rotation)) { raise_error("draw-texture-pro fifth argument (rotation) is not a flonum"); }

    Rectangle source_rect = rect_from_list(source_rect_scm, "draw-texture-pro");
    Rectangle dest_rect = rect_from_list(dest_rect_scm, "draw-texture-pro");
    Vector2 origin = vector2_from_pair(origin_scm, "draw-texture-pro", "origin");
    Color tint = color_from_list(tint_scm, "draw-texture-pro");
    DrawTexturePro(*(Texture2D*)texture_obj->c_wrapped.data, source_rect, dest_rect, origin, GET_FLONUM(rotation), tint);

    return VOID;
}

value set_config_flags(environment env, enum call_flags flags, int nargs, ...) {
    unsigned int fs = 0;

    init_args();
    for (int i = 0; i < nargs; ++i) {
        value f = next_arg();
        if (!IS_FIXNUM(f)) { raise_error("set-config-flags arguments must be integers"); }
        fs |= GET_FIXNUM(f);
    }
    free_args();

    SetConfigFlags(fs);

    return VOID;
}
