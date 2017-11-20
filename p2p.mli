open Lwt_io

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
 * pulling.*)
type t

(* [broadcast m t] sends the given message [m] to all peers of the node [t]. *)
val broadcast : Message_types.message -> t -> unit Lwt.t

(* [create s] makes a p2p node from a list of peers in the file with name [s]. 
 * *)
val create : ?port:int -> string -> t

(* [create s] makes a p2p node from a list of peers in the file with name [s]. 
 * *)
val create_from_list : (string*int) list -> t

(* [peer_stream t] is a stream of peers of [t]. Elements of the stream are 
 * tuples of an input stream and an output stream.*)
val peer_stream : t -> (input channel * output channel) Lwt_stream.t

val num_of_peers : t -> int

val start_server: ?port:int -> t -> unit Lwt.t

val connect_to_known_peers: t -> unit Lwt.t

val in_message_stream : t -> Message_types.message Lwt_stream.t 