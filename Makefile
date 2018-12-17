all: build

build:
	cd src && make all

build-deps:
	opam install dune menhir

clean:
	   cd src && make clean
