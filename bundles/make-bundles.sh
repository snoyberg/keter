#!/bin/bash -ex

pushd `dirname $0`

rm ./*.keter -f

pushd ./foo
tar czfv ../foo.keter *
popd

pushd ./bar
tar czfv ../bar.keter *
popd 

pushd ./baz
tar czfv ../baz.keter *
popd

pushd ./qux
tar czfv ../qux.keter *
popd

cp ./*.keter /opt/keter/incoming/
