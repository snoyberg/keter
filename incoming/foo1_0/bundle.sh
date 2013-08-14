#!/bin/bash -ex

ghc --make hello.hs
tar czfv ../foo1_0.keter *
