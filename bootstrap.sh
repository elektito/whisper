#!/usr/bin/env sh

set -e

mkdir -p bootstrap
cd bootstrap
rm -rf *

echo "==== v1 ===="
git clone .. --branch=v1 v1 >/dev/null 2>&1
cd v1
make

echo "==== current ===="
cd ..
git clone .. --branch=master current 2>&1 >/dev/null
cd current
cp ../v1/whisper-v1 .
make
cp whisper-v1 ../whisper
