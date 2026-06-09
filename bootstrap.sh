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

echo "==== v3 ===="
cd ..
git clone .. --branch=v3 v3 >/dev/null >/dev/null 2>&1
cd v3
cp ../v2/whisper-v2 .
make

echo "==== v4 ===="
cd ..
git clone .. --branch=v4 v4 >/dev/null >/dev/null 2>&1
cd v4
cp ../v3/whisper-v3 .
make

echo "==== v5 ===="
cd ..
git clone .. --branch=v5 v5 >/dev/null 2>&1
cd v5
cp ../v4/whisper-v4 .
make

echo "==== v6 ===="
cd ..
git clone .. --branch=v6 v6 >/dev/null 2>&1
cd v6
cp ../v5/whisper-v5 .
make

echo "==== current ===="
cd ..
git clone .. --branch=master current >/dev/null 2>&1
cd current
cp ../v6/whisper-v6 .
make
cp whisper-v7 ../whisper
