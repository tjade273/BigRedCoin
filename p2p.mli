open Lwt_io
(* Manages known peers. *)

module type Message_channel = sig
  
  (* The type of an input message channel. *)
  type input

  (* The type of an output message channel. *)
  type output

  (* [write o m] writes the message [m] to the given output channel [o]. *)
  val write : output -> Message_types.message -> unit Lwt.t

  (* [read i] reads a message from the given input channel [i]. *)
  val read : input -> Message_types.message option Lwt.t

end

(* The type of a p2p node. Contains a peer list, and streams for pushing and
 * pulling. *)
type t

(* [broadcast m t] sends the given message [m] to all peers of the node [t]. *)
val broadcast : Message_types.message -> t -> unit

(* [create s] makes a p2p node from a list of peers in the file with name [s]. 
 * *)
val create : string -> t

(* [peer_stream t] is a stream of peers of [t]. Elements of the stream are 
 * tuples of an input stream and an output stream. *)
val peer_stream : t -> (input channel * output channel) Lwt_stream.t
