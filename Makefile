# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: default build install clean uninstall

default: build

build:
	dune build 
test:
	dune runtest

install:
	dune install
# Clean up
clean:
# Remove files produced by dune.
	dune clean 

uninstall:
	dune uninstall wcec-dvk

