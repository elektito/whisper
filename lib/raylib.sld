(define-library (raylib)
  (import (whisper core))

  (begin
    ;; colors
    (define light-gray '(200 200 200 255))
    (define gray '(130 130 130 255))
    (define dark-gray '(80 80 80 255))
    (define yellow '(253 249 0 255))
    (define gold '(255 203 0 55))
    (define orange '(255 161 0 255))
    (define pink '(255 109 194 255))
    (define red '(230 41 55 255))
    (define maroon '(190 33 55 255))
    (define green '(0 228 48 255))
    (define lime '(0 158 47 255))
    (define dark-green '(0 117 44 255))
    (define sky-blue '(102 191 255 255))
    (define blue '(0 121 241 255))
    (define dark-blue '(0 82 172 255))
    (define purple '(200 122 255 255))
    (define violet '(135 60 190 255))
    (define dark-purple '(112 31 126 255))
    (define beige '(211 176 131 255))
    (define brown  '(127 106 79 255))
    (define dark-brown '(76 63 47 255))

    (define white '(255 255 255 255))
    (define black '(0 0 0 255))
    (define blank '(0 0 0 0))
    (define magenta '(255 0 255 255))
    (define ray-white '(245 245 245 255))

    ;; keys
    (define key-null 0)        ; Key: NULL, used for no key pressed

    ;; alphanumeric keys
    (define key-apostrophe 39)  ; Key: '
    (define key-comma 44)       ; Key: ,
    (define key-minus 45)       ; Key: -
    (define key-period 46)      ; Key: .
    (define key-slash 47)       ; Key: /
    (define key-zero 48)        ; Key: 0
    (define key-one 49)         ; Key: 1
    (define key-two 50)         ; Key: 2
    (define key-three 51)       ; Key: 3
    (define key-four 52)        ; Key: 4
    (define key-five 53)        ; Key: 5
    (define key-six 54)         ; Key: 6
    (define key-seven 55)       ; Key: 7
    (define key-eight 56)       ; Key: 8
    (define key-nine 57)        ; Key: 9
    (define key-semicolon 59)   ; Key: ;
    (define key-equal 61)       ; Key: =
    (define key-a 65)       ; Key: A | a
    (define key-b 66)       ; Key: B | b
    (define key-c 67)       ; Key: C | c
    (define key-d 68)       ; Key: D | d
    (define key-e 69)       ; Key: E | e
    (define key-f 70)       ; Key: F | f
    (define key-g 71)       ; Key: G | g
    (define key-h 72)       ; Key: H | h
    (define key-i 73)       ; Key: I | i
    (define key-j 74)       ; Key: J | j
    (define key-k 75)       ; Key: K | k
    (define key-l 76)       ; Key: L | l
    (define key-m 77)       ; Key: M | m
    (define key-n 78)       ; Key: N | n
    (define key-o 79)       ; Key: O | o
    (define key-p 80)       ; Key: P | p
    (define key-q 81)       ; Key: Q | q
    (define key-r 82)       ; Key: R | r
    (define key-s 83)       ; Key: S | s
    (define key-t 84)       ; Key: T | t
    (define key-u 85)       ; Key: U | u
    (define key-v 86)       ; Key: V | v
    (define key-w 87)       ; Key: W | w
    (define key-x 88)       ; Key: X | x
    (define key-y 89)       ; Key: Y | y
    (define key-z 90)       ; Key: Z | z
    (define key-left-bracket 91)    ; Key: [
    (define key-backslash 92)       ; Key: '\'
    (define key-right-bracket 93)   ; Key: ]
    (define key-grave 96)           ; Key: `

    ;; Function keys
    (define key-space 32)      ; Key: Space
    (define key-escape 256)    ; Key: Esc
    (define key-enter 257)     ; Key: Enter
    (define key-tab 258)       ; Key: Tab
    (define key-backspace 259) ; Key: Backspace
    (define key-insert 260)    ; Key: Ins
    (define key-delete 261)    ; Key: Del
    (define key-right 262)     ; Key: Cursor right
    (define key-left 263)      ; Key: Cursor left
    (define key-down 264)      ; Key: Cursor down
    (define key-up 265)        ; Key: Cursor up
    (define key-page-up 266)   ; Key: Page up
    (define key-page-down 267) ; Key: Page down
    (define key-home 268)      ; Key: Home
    (define key-end 269)       ; Key: End
    (define key-caps-lock 280) ; Key: Caps lock
    (define key-scroll-lock 281)   ; Key: Scroll down
    (define key-num-lock 282)      ; Key: Num lock
    (define key-print-screen 283)  ; Key: Print screen
    (define key-pause 284)   ; Key: Pause
    (define key-f1 290)      ; Key: F1
    (define key-f2 291)      ; Key: F2
    (define key-f3 292)      ; Key: F3
    (define key-f4 293)      ; Key: F4
    (define key-f5 294)      ; Key: F5
    (define key-f6 295)      ; Key: F6
    (define key-f7 296)      ; Key: F7
    (define key-f8 297)      ; Key: F8
    (define key-f9 298)      ; Key: F9
    (define key-f10 299)     ; Key: F10
    (define key-f11 300)     ; Key: F11
    (define key-f12 301)     ; Key: F12
    (define key-left-shift 340)    ; Key: Shift left
    (define key-left-control 341)  ; Key: Control left
    (define key-left-alt 342)      ; Key: Alt left
    (define key-left-super 343)    ; Key: Super left
    (define key-right-shift 344)   ; Key: Shift right
    (define key-right-control 345) ; Key: Control right
    (define key-right-alt 346)     ; Key: Alt right
    (define key-right-super 347)   ; Key: Super right
    (define key-kb-menu 348)       ; Key: KB menu

    ;; keypad keys
    (define key-kp-0 320)      ; Key: Keypad 0
    (define key-kp-1 321)      ; Key: Keypad 1
    (define key-kp-2 322)      ; Key: Keypad 2
    (define key-kp-3 323)      ; Key: Keypad 3
    (define key-kp-4 324)      ; Key: Keypad 4
    (define key-kp-5 325)      ; Key: Keypad 5
    (define key-kp-6 326)      ; Key: Keypad 6
    (define key-kp-7 327)      ; Key: Keypad 7
    (define key-kp-8 328)      ; Key: Keypad 8
    (define key-kp-9 329)      ; Key: Keypad 9
    (define key-kp-decimal 330)    ; Key: Keypad .
    (define key-kp-divide 331)     ; Key: Keypad /
    (define key-kp-multiply 332)   ; Key: Keypad *
    (define key-kp-subtract 333)   ; Key: Keypad -
    (define key-kp-add 334)        ; Key: Keypad +
    (define key-kp-enter 335)      ; Key: Keypad Enter
    (define key-kp-equal 336)      ; Key: Keypad =

    ;; android key buttons
    (define key-back 4)         ; Key: Android back button
    (define key-menu 5)         ; Key: Android menu button
    (define key-volume-up 24)   ; Key: Android volume up button
    (define key-volume-down 25) ; Key: Android volume down button
    )

  (export light-gray
          gray
          dark-gray
          yellow
          gold
          orange
          pink
          red
          maroon
          green
          lime
          dark-green
          sky-blue
          blue
          dark-blue
          purple
          violet
          dark-purple
          beige
          brown
          dark-brown

          white
          black
          blank
          magenta
          ray-white

          key-null
          key-apostrophe
          key-comma
          key-minus
          key-period
          key-slash
          key-zero
          key-one
          key-two
          key-three
          key-four
          key-five
          key-six
          key-seven
          key-eight
          key-nine
          key-semicolon
          key-equal
          key-a
          key-b
          key-c
          key-d
          key-e
          key-f
          key-g
          key-h
          key-i
          key-j
          key-k
          key-l
          key-m
          key-n
          key-o
          key-p
          key-q
          key-r
          key-s
          key-t
          key-u
          key-v
          key-w
          key-x
          key-y
          key-z
          key-left-bracket
          key-backslash
          key-right-bracket
          key-grave

          key-space
          key-escape
          key-enter
          key-tab
          key-backspace
          key-insert
          key-delete
          key-right
          key-left
          key-down
          key-up
          key-page-up
          key-page-down
          key-home
          key-end
          key-caps-lock
          key-scroll-lock
          key-num-lock
          key-print-screen
          key-pause
          key-f1
          key-f2
          key-f3
          key-f4
          key-f5
          key-f6
          key-f7
          key-f8
          key-f9
          key-f10
          key-f11
          key-f12
          key-left-shift
          key-left-control
          key-left-alt
          key-left-super
          key-right-shift
          key-right-control
          key-right-alt
          key-right-super
          key-kb-menu

          key-kp-0
          key-kp-1
          key-kp-2
          key-kp-3
          key-kp-4
          key-kp-5
          key-kp-6
          key-kp-7
          key-kp-8
          key-kp-9
          key-kp-decimal
          key-kp-divide
          key-kp-multiply
          key-kp-subtract
          key-kp-add
          key-kp-enter
          key-kp-equal

          key-back
          key-menu
          key-volume-up
          key-volume-down
          )

  (c-include "raylib.c")
  (c-export (init-window "init_window" 3 3)
            (close-window "close_window" 0 0)
            (window-should-close "window_should_close" 0 0)

            (clear-background "clear_background" 1 1)
            (begin-drawing "begin_drawing" 0 0)
            (end-drawing "end_drawing" 0 0)

            (set-target-fps "set_target_fps" 1 1)
            (get-frame-time "get_frame_time" 0 0)
            (get-time-us "get_time_us" 0 0)
            (get-fps "get_fps" 0 0)

            (is-key-pressed "is_key_pressed" 1 1)
            (is-key-pressed-repeat "is_key_pressed_repeat" 1 1)
            (is-key-down "is_key_down" 1 1)
            (is-key-released "is_key_released" 1 1)
            (is-key-up "is_key_up" 1 1)

            (draw-pixel "draw_pixel" 3 3)
            (draw-line "draw_line" 5 5)
            (draw-circle "draw_circle" 4 4)
            (draw-rectangle "draw_rectangle" 5 5)

            (draw-fps "draw_fps" 2 2)
            (draw-text "draw_text" 5 5))
  (c-archives "libraylib.a")
  (c-static-flags "-lm -lX11 -lGL")
  (c-so-flags "-lraylib -Wl,-rpath '$ORIGIN' -lm -lX11 -lGL"))
