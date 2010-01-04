OCAML=ocamlopt.opt
OCAMLIB=
#-p -cclib -g

control_module: control_module.ml
	ocamlopt.opt -thread -o control_module -I /usr/lib/ocaml/3.09.1/unix -I /usr/lib/ocaml/3.09.1/threads/ unix.cmxa threads.cmxa control_module.ml

