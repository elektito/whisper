# Whisper: A Scheme to C Compiler

_Whisper_ is a self-hosting scheme-to-c compiler. It targets r7rs-small
compatibility though there are some missing features at the moment.
Here's a short, incomplete overview of what is and isn't supported.

Supported:
 - hygienic macros
 - call/cc
 - exceptions
 - libraries
 - eval and REPL
 
Missing:
 - Most of numeric tower. Only 61-bit fixnums and 32-bit flonums are supported.
 - Unicode. Strings are ASCII only.

Since I intend to play with _Whisper_ and try some gamedev in it, I've
also added some basic raylib bindings. You can simply `(import
(raylib))`. Many of raylib features are missing atm. I will be adding
those as I need them for my own gamedev projects.

## Bootstrapping

A `bootstrap.sh` script is included that allows you to bootstrap the
compiler using gcc. It builds the full version history of the compiler
starting from v1 which can be built from C and then proceeds to v2 and
so on which are written in scheme.
