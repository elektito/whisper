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
compiler. It can bootstrap either from a seed compiler written in C or
from another Scheme implementation. Either way a C compiler is needed.

### Bootstrapping with gcc

In order to bootstrap using gcc (currently the only tested C compiler),
the script clones the v1 branch of whisper, which contains a seed
compiler written in C, and builds v1 using that. It then continues with
v2 and so on, up to the current version.

In order to bootstrap using this method, simply run `./bootstrap.sh`.

### Bootstrapping with Guile

Currently, Guile is the only Scheme implementation which can bootstrap
whisper. This does not require building the entire version history so
it's considerably faster than bootstrapping with gcc alone. Simply run
`./bootstrap.sh --scheme guile`. Guile 3.0.9 was used to test this.
