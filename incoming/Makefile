# Keter Configuration Examples
#
#  See README.md for build details


all: foo.keter foo1_0.keter websockets
.PHONY: all nc.keter

# Simple targets
foo: foo.keter
foo1_0: foo1_0.keter
websockets: websockets.keter
nc: nc.keter


foo.keter: ./foo/*.hs ./foo/config/*.yaml
	@echo "Build foo bundle"
	ghc --make ./foo/hello.hs
	strip ./foo/hello
	cd foo; tar czfv ../foo.keter *

nc.keter:
	cd nc; tar czfv ../nc.keter *

foo1_0.keter: ./foo1_0/*.hs ./foo1_0/config/*.yaml
	@echo "Build foo1_0"
	ghc --make ./foo1_0/hello.hs
	ghc --make ./foo1_0/worker.hs
	strip ./foo1_0/hello
	strip ./foo1_0/worker
	cd foo1_0; tar czfv ../foo1_0.keter *


# websockets needs extra dependencies:
#
#  $ cabal install yesod-websockets lifted-base
websockets.keter: ./websockets/*.hs ./websockets/config/*.yaml
	@echo "Building websockets example."
	ghc --make ./websockets/chat.hs
	strip ./websockets/chat
	cd websockets; tar czfv ../websockets.keter *


clean:
	find . -type f -name '*.keter' -delete
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.o' -delete
	find . -type f -name '*.dyn_hi' -delete
	find . -type f -name '*.dyn_o' -delete
