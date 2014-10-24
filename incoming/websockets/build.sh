#!/bin/bash -ex

ghc --make chat.hs
tar czfv websockets.keter chat config/keter.yaml
cp websockets.keter ..
