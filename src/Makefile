all: cateq build-web

cateq:
	ocamlbuild cateq.byte -use-menhir
	mv cateq.byte cateq

build-web:
	ocamlbuild -use-menhir -use-ocamlfind -plugin-tag 'package(js_of_ocaml-ocamlbuild)' -pkgs js_of_ocaml,js_of_ocaml-ppx web/cateqweb.byte
	js_of_ocaml cateqweb.byte --opt 3 -o cateqweb.js
	mv cateqweb.js ../web/cateqweb.js

clean:
	rm -r _build
	rm -f cateq
	rm -f ../web/cateqweb.js

cmo:
	ocamlbuild cateq.cmo -use-menhir

ci:
	git ci . -m "Prog."
	git push

.PHONY: cateq build-web
