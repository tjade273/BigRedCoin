.PHONY: all test clean

all: test

src/message_pb.ml: src/message.proto
	cd src && ocaml-protoc -binary -int32_type int_t -int64_type int_t -ml_out . message.proto

test: src/message_pb.ml
	ocamlbuild -use-ocamlfind run_tests.byte && ./run_tests.byte

clean:
	rm -rf _build *.native *.byte *.db src/message_{pb,types}.ml*
	
repl:
	ocamlbuild -use-ocamlfind run_repl.byte && ./run_repl.byte
	
