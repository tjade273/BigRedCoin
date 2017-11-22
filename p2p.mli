open Lwt_io

module type Message_channel = sig
  (* The type of an input message channel. *)
  type input

  (* The type of an output message channel. *)
  type output

  (* [write oc msg] writes the message [msg] to the given output channel [oc]. *)
  val write : output -> Message_types.message -> int Lwt.t

  (* [read ic] reads a message from the given input channel [ic]. *)
  val read : input -> Message_types.message option Lwt.t

  (* [close_in ic] closes the given input channel [ic]. *)
  val close_in : input -> unit Lwt.t

  (* [close_out oc] closes the given output channel [oc]. *)
  val close_out : output -> unit Lwt.t
end

module BRCMessage_channel : Message_channel

module type BRCPeer_t = sig
  (* The type of a peer connection in the node *)
  type peer_connection

  (* The type of a peer *)
  type peer

  (* [null_peer ()] is a blank peer with no address. *)
  val null_peer : unit -> peer

  (* [p1 <=> p2] is True iff [p1] and [p2] have the same address. *)
  val (<=>) : peer -> peer -> bool

  (* [addr p] is the [Unix.sockaddr] address of the given peer [p]. *)
  val addr : peer -> Unix.sockaddr

  (* [s_addr p] is the string representation of the address of the given
   * peer [p]. *)
  val s_addr : peer -> string

  (* [str conn] is the string represenation of the address of the given peer
   * connection [conn]. *)
  val str : peer_connection -> string

  (* [ic conn] is the input channel associated with the given peer connection
   * [conn]. *)
  val ic : peer_connection -> BRCMessage_channel.input

  (* [ic conn] is the output channel associated with the given peer connection
   * [conn]. *)
  val oc : peer_connection -> BRCMessage_channel.output
end

module BRCPeer : BRCPeer_t

(* The type of a p2p node. Contains a peer list, and streams for pushing and
 * pulling.*)
type t

(* [broadcast m t] sends the given message [m] to all peers of the node [t]. *)
val broadcast : Message_types.message -> t -> unit Lwt.t

(* [create s] makes a p2p node from a list of peers in the file with name [s].
 * *)
val create : ?port:int -> string -> t Lwt.t

(* [create s] makes a p2p node from a list of peers in the file with name [s].
 * *)
val create_from_list : ?port:int -> (string * int * (Unix.tm option)) list -> t Lwt.t

(* [peer_stream t] is a stream of peers of [t]. Elements of the stream are
 * tuples of an input stream and an output stream.*)
val peer_stream : t -> BRCPeer.peer_connection Lwt_stream.t

(*val connect_to_known_peers: t -> unit Lwt.t
(*val connect_to_known_peers: t -> unit Lwt.t*)*)

val shutdown : t -> unit Lwt.t

val close_peer_connection: t -> BRCPeer.peer_connection -> unit Lwt.t

val handle : (BRCPeer.peer_connection -> (bool*('a Lwt.t)) Lwt.t) -> t ->
  BRCPeer.peer_connection -> 'a Lwt.t

val server_port : t -> int
val (@<>) : (t*BRCPeer.peer_connection) -> (BRCPeer.peer_connection ->
  (bool*('a Lwt.t)) Lwt.t) -> 'a Lwt.t

(*Create seperate stream for incomming connections*)
