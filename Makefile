all: test

secp256k1/Makefile: secp256k1/autogen.sh
	cd secp256k1 && ./autogen.sh && ./configure

build_libs: secp256k1/Makefile
	$(MAKE) -C ./secp256k1

test: build_libs
	ocamlbuild -use-ocamlfind crypto_test.byte && ./crypto_test.byte
