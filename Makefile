EXAMPLE ?= constrained-fit

.PHONY: build run list test idrcad-examples constrained-fitting partial-fit front-panel repl clean

build:
	idris2 --build idrcad.ipkg

run: build
	./build/exec/idrcad $(EXAMPLE)

list: build
	./build/exec/idrcad --list

test: build
	idris2 --build idrcad-tests.ipkg
	./build/exec/idrcad-tests
	$(MAKE) idrcad-examples

idrcad-examples: build
	@test "$$(find examples/idrcad -type f -name '*.idrcad' | wc -l)" -eq 51
	find examples/idrcad -type f -name '*.idrcad' -print0 | sort -z | \
		xargs -0 -n1 ./build/exec/idrcad check

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

front-panel: build
	IDRIS2_PREFIX="$(CURDIR)/build/example-prefix" idris2 --install idrcad.ipkg
	cd examples/front-panel && \
		IDRIS2_PREFIX="$(CURDIR)/build/example-prefix" \
		idris2 --build front-panel.ipkg

repl:
	idris2 src/Main.idr

clean:
	idris2 --clean idrcad.ipkg
	idris2 --clean idrcad-tests.ipkg
	cd examples/constrained-fitting && idris2 --clean constrained-fitting.ipkg
	cd examples/partial-fit && idris2 --clean partial-fit.ipkg
	cd examples/front-panel && idris2 --clean front-panel.ipkg
