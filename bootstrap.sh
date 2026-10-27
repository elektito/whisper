#!/usr/bin/env sh

# usage: ./bootstrap.sh [--scheme <impl>]
#
# without arguments, builds every version from v1 (which has a C seed
# compiler) up to the current one. with --scheme, runs the current
# compiler's sources on another scheme implementation instead, to build
# the first stage directly.

set -e

CURRENT_V=40
LAST_V=$((CURRENT_V - 1))

SCHEME=
if [ "$1" = "--scheme" ]; then
  if [ -z "$2" ]; then
    echo "missing argument to --scheme" >&2
    exit 1
  fi
  SCHEME=$2
elif [ -n "$1" ]; then
  echo "usage: $0 [--scheme <impl>]" >&2
  exit 1
fi

# the command that runs main.scm on each supported implementation, with
# the stand-in (whisper) library on its library path.
case "$SCHEME" in
  "") ;;
  guile) RUN_MAIN="guile --r7rs -x .sld -L bootstrap-scheme main.scm" ;;
  *)
    echo "unsupported scheme: $SCHEME (supported: guile)" >&2
    exit 1
    ;;
esac

mkdir -p bootstrap
cd bootstrap
rm -rf *

# ensure local branch exists in parent repo before cloning from it
ensure_local_branch() {
  git -C .. branch "$1" "origin/$1" >/dev/null 2>&1 || true
}

if [ -n "$SCHEME" ]; then
  echo "==== current, from $SCHEME ===="
  git clone .. --branch=master current >/dev/null 2>&1
  cd current

  # these stand in for the three steps in the Makefile that use the
  # previous version. the sources are the current ones, so they use the
  # current core files rather than prev/.
  mkdir -p stage0-lib
  $RUN_MAIN lib/whisper/whisper.sld -l -o stage0-lib/whisper -C .
  $RUN_MAIN lib/whisper/compiler.sld -l -o stage0-lib/compiler -C . -L stage0-lib
  $RUN_MAIN main.scm -o stage0 -C . -L stage0-lib
  make
  cp whisper-v$CURRENT_V ../whisper
  cd ..
  exit 0
fi

echo "==== v1 ===="
ensure_local_branch v1
git clone .. --branch=v1 v1 >/dev/null 2>&1
cd v1
make
cd ..

for i in $(seq 2 "$LAST_V"); do
  prev=$((i - 1))
  echo "==== v$i ===="
  ensure_local_branch "v$i"
  git clone .. --branch=v$i v$i >/dev/null 2>&1
  cd v$i
  cp ../v$prev/whisper-v$prev .
  make
  cd ..
done

echo "==== current ===="
git clone .. --branch=master current >/dev/null 2>&1
cd current
cp ../v$LAST_V/whisper-v$LAST_V .
make
cp whisper-v$CURRENT_V ../whisper
cd ..
