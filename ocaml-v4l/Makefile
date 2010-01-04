CC=gcc
OCAMLLIB= -cclib -L. -ccopt -fPIC
CFLAGS=-fPIC
CPPFLAGS=-fPIC
fg.o:	fg.c fg.h
	gcc -fPIC    -c -o fg.o fg.c

fgcamlstubs.o:	fgcamlstubs.c fg.h
	ocamlopt.opt -c grabcaml.mli
	ocamlopt.opt -ccopt -fPIC -I /usr/lib/ocaml/caml -c fgcamlstubs.c 
grabcaml.cmx: grabcaml.ml fgcamlstubs.o
	ocamlopt.opt -ccopt -fPIC -c grabcaml.ml ${OCAMLIB} 
sobel.cmx: sobel.ml
	ocamlopt.opt -c sobel.ml
grabcaml.cmxa: fgcamlstubs.o grabcaml.cmx fg.o
	ocamlmklib -o grabcaml grabcaml.cmx -ccopt -fPIC
	ocamlmklib -o grabcaml fgcamlstubs.o fg.o  ${OCAMLLIB}
grabcaml_ex: grabcaml_ex.ml grabcaml.cmxa sobel.cmx grabcaml.cmx
	ocamlopt.opt -I . -o grabcaml_ex unix.cmxa  grabcaml.cmxa sobel.cmx grabcaml_ex.ml ${OCAMLLIB} -ccopt -fPIC
clean:
	rm *.o *.cmx *.cmi *.cma *.cmxa grabcaml_ex
