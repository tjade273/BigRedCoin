.PHONY: all test clean

all: test

src/message_pb.ml:
	cd src && ocaml-protoc -binary -int32_type int_t -int64_type int_t -ml_out . message.proto

test: src/message_pb.ml
	ocamlbuild -use-ocamlfind run_tests.byte && ./run_tests.byte

clean:
	rm -rf _build *.native *.byte *.db 
	
repl:
	ocamlbuild -use-ocamlfind brc_repl.byte && ./brc_repl.byte

repl_test:
	ocamlbuild -use-ocamlfind repl_test.byte && ./repl_test.byte
