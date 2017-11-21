test:
	ocamlbuild -use-ocamlfind crypto_test.byte && ./crypto_test.byte
