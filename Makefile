GH_NAME:=issueform2md
BIN_NAME:=gh-${GH_NAME}
MAIN:=main.exe

build: FORCE
	opam exec -- dune build ./bin

clean:
	opam exec -- dune clean
	rm -f ${MAIN} ${BIN_NAME}

test: build
	opam exec -- dune test

setup:
	opam install ./gh-issueform2md.opam --yes --deps-only --with-test

install_as_extension: build
	cp -f ${MAIN} ${BIN_NAME}
	gh extension remove ${GH_NAME} || echo
	gh extension install .

install: setup install_as_extension

format:
	opam exec -- dune fmt

lint:
	opam lint

FORCE:
.PHONY: FORCE
