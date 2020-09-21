all: cateq web

cateq:
	ocamlbuild cateq.byte -use-menhir
	cp cateq.byte cateq

web:
	ocamlbuild -use-menhir -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml-ppx web/cateqweb.byte
	js_of_ocaml cateqweb.byte --opt 3 -o cateqweb.js
	mv cateqweb.js ../web/cateqweb.js

clean:
	ocamlbuild -clean
	rm -f cateq

.PHONY: cateq web
