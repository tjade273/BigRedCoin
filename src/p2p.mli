open Lwt_io


type log_level = 
  | DEBUG
  | SILENT

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

  (* [new_peer addr port] is a peer with the given address [addr] and [port]. *)
  val new_peer : string -> int -> peer

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

(* [broadcast msg p2p] sends the given message [msg] to all peers of the
 * node [p2p]. *)
val broadcast : Message_types.message -> t -> unit Lwt.t

(* [create p f] makes a p2p node with the port [p] and the a list of peers
 * in the file with name [f]. *)
val create : ?port:int -> string -> t Lwt.t

(* [create_from_list p peers] makes a p2p node with port [p] and the list of
 * [peers] *)
val create_from_list : ?port:int -> (string * int * (Unix.tm option)) list
  -> t Lwt.t

(* [peer_stream p2p] is a stream of peers of [t]. Elements of the stream are
 * tuples of an input stream and an output stream.*)
val peer_stream : t -> BRCPeer.peer_connection Lwt_stream.t

(* [server_port p2p] is the port linked to the [p2p] node. *)
val server_port : t -> int

(* [(p2p,conn) @<> f] is syntactic sugar for [handle f p2p conn]. *)
val (@<>) : (t*BRCPeer.peer_connection) -> (BRCPeer.peer_connection ->
                (bool*('a Lwt.t)) Lwt.t) -> 'a Lwt.t

(* [shutdown p2p] shuts down the given [p2p] node by closing all its connections,
 * saving the updated list of known peers to file, and shuting down the server. *)
val shutdown : ?save:bool -> t -> unit Lwt.t

(*[set_log_level p2p level] set the log level of a p2p instance to [level]*)
val set_log_level : t -> log_level -> unit

(*[known_peers p2p]* returns a list of nodes [p2p] known peers*)
val known_peers : t -> BRCPeer.peer list 