.PHONY = all,clean

all:
	rm -f *.native *.byte
	ocamlbuild -use-ocamlfind -cflag -g -lflag -g main.byte -pkgs str
