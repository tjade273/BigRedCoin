open Lwt_io
(* Manages known peers. *)

module type Message_channel = sig
  
  (* The type of an input message channel. *)
  type input

  (* The type of an output message channel. *)
  type output

  (* Write the input message to the given output channel. *)
  val write : output -> Message_types.message -> unit Lwt.t

  (* Read an input message from the given input channel. *)
  val read : input -> Message_types.message option Lwt.t

end

(* The type of a p2p node. Contains a peer list, streams for pushing and
 * pulling. *)
type t

(* Sends the given message to all peers of this node. *)
val broadcast : Message_types.message -> t -> unit

(* Create a p2p node from a list of peers in the given file. *)
val create : string -> t

(* A stream of peers. Elements of the stream are tuples of an input stream and
 * an output stream. *)
val peer_stream : t -> (input channel * output channel) Lwt_stream.t
