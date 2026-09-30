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

## SRFI Support

The following SRFIs are supported:

 - [SRFI 1](https://srfi.schemers.org/srfi-1/srfi-1.html) - List Library
 - [SRFI 8](https://srfi.schemers.org/srfi-8/srfi-8.html) - receive
 - [SRFI 48](https://srfi.schemers.org/srfi-48/srfi-48.html) - Intermediate Format Strings
 - [SRFI 69](https://srfi.schemers.org/srfi-69/srfi-69.html) - Basic Hash Tables
 - [SRFI 111](https://srfi.schemers.org/srfi-111/srfi-111.html) - Boxes
 - [SRFI 133](https://srfi.schemers.org/srfi-133/srfi-133.html) - Vector Library
 - [SRFI 151](https://srfi.schemers.org/srfi-151/srfi-151.html) - Bitwise Operations
 
## Bootstrapping

A `bootstrap.sh` script is included that allows you to bootstrap the
compiler using gcc. It builds the full version history of the compiler
starting from v1 which can be built from C and then proceeds to v2 and
so on which are written in scheme.
