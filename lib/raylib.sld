(define-library (raylib)
  (import (whisper core))

  (begin
    (define light-gray '(200 200 200 255))
    (define gray '(130 130 130 255))
    (define dark-gray '(80 80 80 255))
    (define yellow '(253 249 0 255))
    (define gold '(255 203 0 55))
    (define orange '(255 161 0 255))
    (define pink '(255 109 194 255))
    (define red '(230 41 55 255))
    (define maroon '(190 33 55 255))
    (define gree '(0 228 48 255))
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
    (define ray-white '(245 245 245 255)))

  (export light-gray
          gray
          dark-gray
          yellow
          gold
          orange
          pink
          red
          maroon
          gree
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
          ray-white)

  (c-include "raylib.c")
  (c-export (init-window "init_window" 3 3)
            (close-window "close_window" 0 0)
            (window-should-close "window_should_close" 0 0)

            (clear-background "clear_background" 1 1)
            (begin-drawing "begin_drawing" 0 0)
            (end-drawing "end_drawing" 0 0))
  (c-archives "libraylib.a")
  (c-static-flags "-lm -lX11 -lGL")
  (c-so-flags "-lraylib -Wl,-rpath '$ORIGIN' -lm -lX11 -lGL"))
