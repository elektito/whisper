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

echo "==== v7 ===="
cd ..
git clone .. --branch=v7 v7 >/dev/null 2>&1
cd v7
cp ../v6/whisper-v6 .
make

echo "==== v8 ===="
cd ..
git clone .. --branch=v8 v8 >/dev/null 2>&1
cd v8
cp ../v7/whisper-v7 .
make

echo "==== v9 ===="
cd ..
git clone .. --branch=v9 v9 >/dev/null 2>&1
cd v9
cp ../v8/whisper-v8 .
make

echo "==== v10 ===="
cd ..
git clone .. --branch=v10 v10 >/dev/null 2>&1
cd v10
cp ../v9/whisper-v9 .
make

echo "==== v11 ===="
cd ..
git clone .. --branch=v11 v11 >/dev/null 2>&1
cd v11
cp ../v10/whisper-v10 .
make

echo "==== v12 ===="
cd ..
git clone .. --branch=v12 v12 >/dev/null 2>&1
cd v12
cp ../v11/whisper-v11 .
make

echo "==== v13 ===="
cd ..
git clone .. --branch=v13 v13 >/dev/null 2>&1
cd v13
cp ../v12/whisper-v12 .
make

echo "==== v14 ===="
cd ..
git clone .. --branch=v14 v14 >/dev/null 2>&1
cd v14
cp ../v13/whisper-v13 .
make

echo "==== current ===="
cd ..
git clone .. --branch=master current >/dev/null 2>&1
cd current
cp ../v14/whisper-v14 .
make
cp whisper-v15 ../whisper
