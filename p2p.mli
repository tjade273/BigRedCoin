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

  val close_in : input -> unit Lwt.t
  val close_out : output -> unit Lwt.t
end


module BRCMessage_channel : Message_channel 

module type BRCPeer_t = sig 
  type peer_connection  
  val ic : peer_connection -> BRCMessage_channel.input
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
val create : ?port:int -> string -> t

(* [create s] makes a p2p node from a list of peers in the file with name [s]. 
 * *)
val create_from_list : ?port:int -> (string*int) list -> t Lwt.t

(* [peer_stream t] is a stream of peers of [t]. Elements of the stream are 
 * tuples of an input stream and an output stream.*)
val peer_stream : t -> BRCPeer.peer_connection Lwt_stream.t

(*val connect_to_known_peers: t -> unit Lwt.t
(*val connect_to_known_peers: t -> unit Lwt.t*)*)

val shutdown : t -> unit Lwt.t

val close_peer_connection: t -> BRCPeer.peer_connection -> unit Lwt.t


(*Create seperate stream for incomming connections*)