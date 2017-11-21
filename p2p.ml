open Lwt

exception FailedToConnect of string

let c_MAX_CONNECTIONS = 1024

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

module BRCMessage_channel : Message_channel with 
  type input = Lwt_io.input Lwt_io.channel
                                             and type output = Lwt_io.output Lwt_io.channel 
= struct 
  type input = Lwt_io.input Lwt_io.channel

  type output = Lwt_io.output Lwt_io.channel
  let write oc msg = 
    let encoder = Pbrt.Encoder.create() in
    Message_pb.encode_message msg encoder;
    let buf = Pbrt.Encoder.to_bytes encoder in
    Lwt_io.write_from oc buf 0 (Bytes.length buf) >|= ignore

  let read_raw_msg ic = 
    let timeout = Lwt_unix.sleep 1. >> Lwt.return (0,Bytes.empty) in
    let buf = Bytes.create 2048 in
    let read = 
      let%lwt sz = Lwt_io.read_into ic buf 0 2048 in Lwt.return(sz,buf) in
    Lwt.pick[timeout;read]

  let read ic = 
    match%lwt read_raw_msg ic with
    | (0,_) -> Lwt.return_none
    | (_,buf) -> 
      Lwt.return_some (Message_pb.decode_message (Pbrt.Decoder.of_bytes buf))
  let close_in ic = 
    Lwt_log.notice "Closing input channel" >>
    Lwt_io.close ic
  let close_out oc = 
    Lwt_log.notice "Closing output channel" >>    
    Lwt_io.close oc 
end

module type BRCPeer_t = sig 
  type peer_connection  
  val ic : peer_connection -> BRCMessage_channel.input
  val oc : peer_connection -> BRCMessage_channel.output
end

module BRCPeer = struct
  type peer_connection = {addr:string;
                          ic:BRCMessage_channel.input;
                          oc:BRCMessage_channel.output;}

  let ic peer = peer.ic
  let oc peer = peer.oc
end 

open BRCPeer
module PeerTbl = struct
  include Hashtbl
  let remove tbl addr =
    match find_opt tbl addr with
    | Some peer -> remove tbl addr; 
      BRCMessage_channel.close_in peer.ic >> 
      BRCMessage_channel.close_out peer.oc
    | None -> Lwt.return_unit

  let add tbl peer =
    remove tbl peer.addr >>
    Lwt.return @@ add tbl peer.addr peer
end
type t = {
  connections:(string,peer_connection) PeerTbl.t;
  handled_connections:(string,peer_connection) Hashtbl.t;
  known_peers:(string*int) list;
  server:Lwt_io.server option;
  port:int}

let remove_known_peer p2p addr = 
  List.remove_assoc p2p.known_peers addr
let remove_handle_connection p2p peer = 
  Hashtbl.remove p2p.handled_connections peer.addr;
  Lwt.return_unit  

let close_peer_connection p2p (peer:peer_connection) =
  PeerTbl.remove p2p.connections peer.addr

let handle f p2p peer  = 
  Hashtbl.add p2p.handled_connections peer.addr peer;
  let%lwt (close,res) = f peer in
  if close then 
    close_peer_connection p2p peer >> res
  else
    remove_handle_connection p2p peer >> res

let (@<>) (p2p,peer) f = handle f p2p peer  
let peer_open inet p2p = 
  PeerTbl.mem p2p.connections inet


let get_connected_peer inet p2p = 
  PeerTbl.find p2p.connections inet
let s_inet addr port = 
  addr ^":"^(string_of_int port)
let socket_addr_to_string addr = 
  match addr with 
  | Lwt_unix.ADDR_UNIX addr -> addr
  | Lwt_unix.ADDR_INET (addr,port) -> (Unix.string_of_inet_addr addr) ^ 
                                      ":" ^ (string_of_int port) 
let decode_message bytes = 
  (Message_pb.decode_message (Pbrt.Decoder.of_bytes bytes))

let encode_message bytes = 
  let encoder = Pbrt.Encoder.create() in
  Message_pb.encode_message bytes encoder;
  Pbrt.Encoder.to_bytes encoder 

let initiate_connection peer_addr =
  Lwt_log.notice ("Attempting to initiate connection: " ^ socket_addr_to_string peer_addr) >> 
  Lwt.catch (
      fun () -> let%lwt (ic, oc)  = Lwt_io.open_connection peer_addr in
        Lwt.return_some {addr=(socket_addr_to_string peer_addr);ic=ic;oc=oc})
      (fun e -> Lwt.return_none)

let connect_to_peer ip port p2p =
  let target = (s_inet ip port) in 
  if (peer_open target p2p) then 
    let peer = (get_connected_peer target p2p) in
    Lwt.return_some peer
  else
    let addr = Unix.(ADDR_INET (Unix.inet_addr_of_string ip, port)) in
      match%lwt initiate_connection addr with
      | Some peer -> 
        PeerTbl.add p2p.connections peer 
        >> Lwt.return_some peer
      | None -> Lwt.return_none

let connect_and_send addr port msg p2p = 
  match%lwt connect_to_peer addr port p2p with 
  | Some peer -> BRCMessage_channel.write peer.oc msg 
    >> Lwt_log.notice ("Wrote Message to: " ^ peer.addr)
  | None -> Lwt.return_unit
let send_raw bytes size oc = 
  Lwt_io.write_from_exactly oc bytes 0 size

let broadcast (msg:Message_types.message) (p2p:t) = 
  List.fold_left (fun acc (addr,port)  -> 
      (connect_and_send addr port msg p2p)<&>acc) 
    Lwt.return_unit p2p.known_peers
let handle_new_peer_connection p2p addr (ic,oc) =
  if (Hashtbl.length p2p.connections < c_MAX_CONNECTIONS) then 
    Lwt_log.notice("Got new peer @ " ^ socket_addr_to_string addr) >> 
    let sock_addr = socket_addr_to_string addr in
    let peer = {addr=sock_addr;ic=ic;oc=oc} in
    PeerTbl.add p2p.connections peer
  else
    BRCMessage_channel.close_in ic >> BRCMessage_channel.close_out oc

let create ?port:(port=4000) peer_file = 
  failwith "Unimplemented create"

let rec connect_to_a_peer p2p ?peers:(peers=p2p.known_peers) () = 
  if List.length peers = 0 then
    Lwt.return_none
  else 
    let random = Random.int (List.length p2p.known_peers) in
    let (peer_addr,port) = List.nth peers random in
    let target_addr = (s_inet peer_addr port) in
    if (PeerTbl.mem p2p.connections target_addr) then
      let peer = PeerTbl.find p2p.connections target_addr in 
      Lwt.return_some peer
    else
      let timeout = Lwt_unix.sleep 2. >> Lwt.return_none in  
      let conn_thread = connect_to_peer peer_addr port p2p in 
      match%lwt Lwt.pick [timeout;conn_thread] with 
      | Some peer -> Lwt.return_some peer
      | None -> 
        let good_peer_lst = List.filter(fun kv -> 
          (fst(kv)) <> peer_addr || (snd(kv)) <> port) peers in
        connect_to_a_peer p2p ~peers:good_peer_lst ()

let tbl_to_list tbl =
  Hashtbl.fold ( fun _ peer lst -> peer::lst) tbl []
let known_peer_stream p2p = 
  Lwt_stream.from(connect_to_a_peer p2p)

let unhandled_connected_peer_stream p2p = 
  let unhandled = Hashtbl.copy p2p.connections in 
  Hashtbl.filter_map_inplace(
    fun addr peer -> 
      if (Hashtbl.mem p2p.handled_connections addr) then
        None
      else
        Some peer
  ) unhandled;
  Lwt_stream.of_list(tbl_to_list unhandled)
let peer_stream p2p =
  Lwt_stream.append (unhandled_connected_peer_stream p2p) (known_peer_stream p2p) 
let start_server port p2p =
  let port = Unix.(ADDR_INET (inet_addr_loopback,port)) in
  let%lwt server =
    Lwt_io.establish_server_with_client_address ~no_close:true port 
      (p2p |> handle_new_peer_connection)
  in
  Lwt.return (server)
let shutdown p2p = 
  match p2p.server with 
  | Some server -> Lwt_io.shutdown_server server
  | None -> Lwt.return_unit  
let add_new_peer addr_port p2p = 
  List.append p2p.known_peers addr_port
let create_from_list ?port:(p=4000) (peer_list:(string*int) list) = 
  let p2p = {server=None;connections=(PeerTbl.create 20);known_peers=peer_list;
             handled_connections=(Hashtbl.create 20);port=p} in
  let%lwt server = start_server p p2p in
  Lwt.return {p2p with server = (Some server)} 
let server_port p2p = 
  p2p.port



