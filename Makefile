.PHONY: all test clean

all: test

message_pb.ml:
	ocaml-protoc -binary -int32_type int_t -int64_type int_t -ml_out . message.proto

test: message_pb.ml
	ocamlbuild -use-ocamlfind crypto_test.byte && ./crypto_test.byte

clean:
	rm -rf _build *.native *.byte
