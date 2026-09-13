#!/usr/bin/env sh

set -e

CURRENT_V=31
LAST_V=$((CURRENT_V - 1))

mkdir -p bootstrap
cd bootstrap
rm -rf *

# ensure local branch exists in parent repo before cloning from it
ensure_local_branch() {
  git -C .. branch "$1" "origin/$1" >/dev/null 2>&1 || true
}

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
