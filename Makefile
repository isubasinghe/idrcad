EXAMPLE ?= constrained-fit

.PHONY: build run list test constrained-fitting partial-fit repl clean

build:
	idris2 --build idrcad.ipkg

run: build
	./build/exec/idrcad $(EXAMPLE)

list: build
	./build/exec/idrcad --list

test:
	idris2 --build idrcad-tests.ipkg
	./build/exec/idrcad-tests

constrained-fitting: build
	IDRIS2_PREFIX="$(CURDIR)/build/example-prefix" idris2 --install idrcad.ipkg
	cd examples/constrained-fitting && \
		IDRIS2_PREFIX="$(CURDIR)/build/example-prefix" \
		idris2 --build constrained-fitting.ipkg

partial-fit: build
	IDRIS2_PREFIX="$(CURDIR)/build/example-prefix" idris2 --install idrcad.ipkg
	cd examples/partial-fit && \
		IDRIS2_PREFIX="$(CURDIR)/build/example-prefix" \
		idris2 --build partial-fit.ipkg

repl:
	idris2 src/Main.idr

clean:
	idris2 --clean idrcad.ipkg
	idris2 --clean idrcad-tests.ipkg
	cd examples/constrained-fitting && idris2 --clean constrained-fitting.ipkg
	cd examples/partial-fit && idris2 --clean partial-fit.ipkg
