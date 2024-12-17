#!/usr/bin/env sh

set -e

mkdir -p bootstrap
cd bootstrap
rm -rf *

echo "==== v1 ===="
git clone .. --branch=v1 v1 >/dev/null >/dev/null 2>&1
cd v1
make

echo "==== v2 ===="
cd ..
git clone .. --branch=v2 v2 >/dev/null >/dev/null 2>&1
cd v2
cp ../v1/whisper-v1 .
make

echo "==== current ===="
cd ..
git clone .. --branch=master current >/dev/null 2>&1
cd current
cp ../v2/whisper-v2 .
make
cp whisper-v3 ../whisper
