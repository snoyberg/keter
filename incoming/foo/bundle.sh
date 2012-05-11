#!/bin/bash -ex

ghc --make hello.hs
tar czfv ../foo.keter *
