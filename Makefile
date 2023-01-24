GH_NAME:=issueform2md
BIN_NAME:=gh-${GH_NAME}
MAIN:=main.exe

DUNE_PROFILE:=
build: FORCE
	opam exec -- dune build ${DUNE_PROFILE} ./bin

DIST:=dist
dist: ${MAIN} FORCE
	mkdir -p ${DIST}
	cp -f ${MAIN} ${DIST}/${BIN_NAME}-linux-amd64

clean:
	opam exec -- dune clean
	rm -rf ${MAIN} ${BIN_NAME} ${DIST}

test: build
	OCAMLRUNPARAM=b opam exec -- dune test

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
