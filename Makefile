GH_NAME:=issueform2md
BIN_NAME:=gh-${GH_NAME}

build: FORCE
	dune build
	cp -f ./_build/install/default/bin/${BIN_NAME} ${BIN_NAME}

clean:
	dune clean

test: build
	dune test

start:
	cat ./test/tests_input.yml | dune exec ${BIN_NAME}

install: build
	gh extension remove ${GH_NAME} || echo
	gh extension install .

repl:
	dune utop lib --watch

format:
	dune fmt

FORCE:
.PHONY: FORCE
