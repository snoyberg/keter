#!/bin/bash -ex

pushd `dirname $0`
echo "In `dirname $0`"
cp ../dist/build/hello/hello ./foo/hello
strip ./foo/hello
cd ./foo
tar czfv ../foo.keter *
