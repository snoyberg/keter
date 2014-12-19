# Keter Configuration Examples
#
#  See README.md for build details


GHC?=ghc
CABAL?=cabal

# TODO don't assume architecture
PKGCONF = ../cabal.sandbox.config
CABAL_ARGS = --sandbox-config-file=$(PKGCONF)


all: foo.keter foo1_0.keter
.PHONY: all

# Simple targets
foo: foo.keter
foo1_0: foo1_0.keter
websockets: websockets.keter


foo.keter: ./foo/*.hs ./foo/config/*.yaml
	@echo "Build foo bundle"
	$(CABAL) $(CABAL_ARGS) exec $(GHC) -- --make ./foo/hello.hs
	strip ./foo/hello
	cd foo; tar czfv ../foo.keter *


foo1_0.keter: ./foo1_0/*.hs ./foo1_0/config/*.yaml
	@echo "Build foo1_0"
	$(CABAL) $(CABAL_ARGS) exec $(GHC) -- --make ./foo1_0/hello.hs
	$(CABAL) $(CABAL_ARGS) exec $(GHC) -- --make ./foo1_0/worker.hs
	strip ./foo1_0/hello
	strip ./foo1_0/worker
	cd foo1_0; tar czfv ../foo1_0.keter *


# websockets needs Yesod and Conduit
websockets.keter: ./websockets/*.hs ./websockets/config/*.yaml
	@echo "Building websockets example."
	$(CABAL) $(CABAL_ARGS) exec $(GHC) -- --make ./websockets/chat.hs
	strip ./websockets/chat
	tar czfv websockets.keter chat config/keter.yaml


clean:
	rm -f *.keter
	find . -type f -name '*.keter' -delete
	find . -type f -name '*.hi' -delete
	find . -type f -name '*.o' -delete
