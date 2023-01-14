GH_NAME:=issueform2md
BIN_NAME:=gh-${GH_NAME}

# to create dummy file before build
$(BIN_NAME):
	echo '#!/bin/sh' > ${BIN_NAME}
	chmod +x ${BIN_NAME}

build: $(BIN_NAME) FORCE
	opam exec -- dune build
	cp -f ./_build/install/default/bin/${BIN_NAME} ${BIN_NAME}

clean:
	opam exec -- dune clean

test: build
	opam exec -- dune test

start:
	cat ./test/tests_input.yml | opam exec -- dune exec ${BIN_NAME}

setup:
	opam install ./gh-issueform2md.opam --yes --deps-only --with-test

install: setup build
	gh extension remove ${GH_NAME} || echo
	gh extension install .

repl:
	opam exec -- dune utop lib --watch

format:
	opam exec -- dune fmt

lint:
	opam lint

FORCE:
.PHONY: FORCE
