(** message.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_get_request : Message_types.get_request -> Pbrt.Encoder.t -> unit
(** [encode_get_request v encoder] encodes [v] with the given [encoder] *)

val encode_get : Message_types.get -> Pbrt.Encoder.t -> unit
(** [encode_get v encoder] encodes [v] with the given [encoder] *)

val encode_transaction_output : Message_types.transaction_output -> Pbrt.Encoder.t -> unit
(** [encode_transaction_output v encoder] encodes [v] with the given [encoder] *)

val encode_transaction_input : Message_types.transaction_input -> Pbrt.Encoder.t -> unit
(** [encode_transaction_input v encoder] encodes [v] with the given [encoder] *)

val encode_transaction : Message_types.transaction -> Pbrt.Encoder.t -> unit
(** [encode_transaction v encoder] encodes [v] with the given [encoder] *)

val encode_block_header : Message_types.block_header -> Pbrt.Encoder.t -> unit
(** [encode_block_header v encoder] encodes [v] with the given [encoder] *)

val encode_block : Message_types.block -> Pbrt.Encoder.t -> unit
(** [encode_block v encoder] encodes [v] with the given [encoder] *)

val encode_peer : Message_types.peer -> Pbrt.Encoder.t -> unit
(** [encode_peer v encoder] encodes [v] with the given [encoder] *)

val encode_post : Message_types.post -> Pbrt.Encoder.t -> unit
(** [encode_post v encoder] encodes [v] with the given [encoder] *)

val encode_message_method : Message_types.message_method -> Pbrt.Encoder.t -> unit
(** [encode_message_method v encoder] encodes [v] with the given [encoder] *)

val encode_message : Message_types.message -> Pbrt.Encoder.t -> unit
(** [encode_message v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_get_request : Pbrt.Decoder.t -> Message_types.get_request
(** [decode_get_request decoder] decodes a [get_request] value from [decoder] *)

val decode_get : Pbrt.Decoder.t -> Message_types.get
(** [decode_get decoder] decodes a [get] value from [decoder] *)

val decode_transaction_output : Pbrt.Decoder.t -> Message_types.transaction_output
(** [decode_transaction_output decoder] decodes a [transaction_output] value from [decoder] *)

val decode_transaction_input : Pbrt.Decoder.t -> Message_types.transaction_input
(** [decode_transaction_input decoder] decodes a [transaction_input] value from [decoder] *)

val decode_transaction : Pbrt.Decoder.t -> Message_types.transaction
(** [decode_transaction decoder] decodes a [transaction] value from [decoder] *)

val decode_block_header : Pbrt.Decoder.t -> Message_types.block_header
(** [decode_block_header decoder] decodes a [block_header] value from [decoder] *)

val decode_block : Pbrt.Decoder.t -> Message_types.block
(** [decode_block decoder] decodes a [block] value from [decoder] *)

val decode_peer : Pbrt.Decoder.t -> Message_types.peer
(** [decode_peer decoder] decodes a [peer] value from [decoder] *)

val decode_post : Pbrt.Decoder.t -> Message_types.post
(** [decode_post decoder] decodes a [post] value from [decoder] *)

val decode_message_method : Pbrt.Decoder.t -> Message_types.message_method
(** [decode_message_method decoder] decodes a [message_method] value from [decoder] *)

val decode_message : Pbrt.Decoder.t -> Message_types.message
(** [decode_message decoder] decodes a [message] value from [decoder] *)
