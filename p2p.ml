open Lwt
open Csv_lwt

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
  type input = Lwt_io.input Lwt_io.channel and
  type output = Lwt_io.output Lwt_io.channel
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
  type peer
  val addr : peer -> Unix.sockaddr
  val s_addr : peer -> string
  val str : peer_connection -> string
  val ic : peer_connection -> BRCMessage_channel.input
  val oc : peer_connection -> BRCMessage_channel.output
end

let socket_addr_to_string addr =
  match addr with
  | Lwt_unix.ADDR_UNIX addr -> addr
  | Lwt_unix.ADDR_INET (addr,port) ->
    (Unix.string_of_inet_addr addr) ^ ":" ^ (string_of_int port)

module BRCPeer = struct
  type peer_connection = {
    addr: Unix.sockaddr;
    ic: BRCMessage_channel.input;
    oc: BRCMessage_channel.output;
  }

  type peer = {
    ip: string;
    port: int;
    mutable timestamp: Unix.tm;
  }

  let addr peer = Unix.(ADDR_INET (inet_addr_of_string peer.ip, peer.port))
  let s_addr peer = peer.ip ^ ":" ^ (string_of_int peer.port)
  let str conn = socket_addr_to_string conn.addr
  let ic conn = conn.ic
  let oc conn = conn.oc
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
    remove tbl (str peer) >>
    Lwt.return @@ add tbl (str peer) peer
end

type t = {
  connections: (string, peer_connection) Hashtbl.t;
  mutable known_peers: peer list;
  server: Lwt_io.server option;
  peer_file: string;
}

let peer_open inet p2p =
  PeerTbl.mem p2p.connections inet

let get_connected_peer inet p2p =
  PeerTbl.find p2p.connections inet

let close_peer_connection p2p peer =
  PeerTbl.remove p2p.connections (str peer)

let s_inet ip port =
  ip ^ ":" ^ (string_of_int port)

let decode_message bytes =
  (Message_pb.decode_message (Pbrt.Decoder.of_bytes bytes))

let encode_message bytes =
  let encoder = Pbrt.Encoder.create() in
  Message_pb.encode_message bytes encoder;
  Pbrt.Encoder.to_bytes encoder

let initiate_connection peer_addr =
  Lwt_log.notice ("Attempting to initiate connection: " ^ (socket_addr_to_string peer_addr)) >>
  let%lwt (ic, oc) = Lwt_io.open_connection peer_addr in
  Lwt.return_some { addr=peer_addr; ic=ic; oc=oc }

let connect_to_peer peer p2p =
  let target = s_addr peer in
  if (peer_open target p2p) then
    Lwt.return_some (get_connected_peer target p2p)
  else
    try
      match%lwt initiate_connection (addr peer) with
      | Some conn ->
        PeerTbl.add p2p.connections conn
        >> Lwt.return_some conn
      | None -> Lwt.return_none
    with
    | _ -> Lwt_log.notice ("Failed to connect to:" ^ target)
      >> Lwt.return_none

let connect_and_send peer msg p2p =
  match%lwt connect_to_peer peer p2p with
  | Some conn -> BRCMessage_channel.write conn.oc msg
    >> Lwt_log.notice ("Wrote Message to: " ^ (str conn))
  | None -> Lwt.return_unit

let send_raw bytes size oc =
  Lwt_io.write_from_exactly oc bytes 0 size

let broadcast (msg:Message_types.message) (p2p:t) =
  List.fold_left (fun acc peer ->
      (connect_and_send peer msg p2p)<&>acc)
    Lwt.return_unit p2p.known_peers

let handle_new_peer_connection p2p addr (ic,oc) =
  if (Hashtbl.length p2p.connections < c_MAX_CONNECTIONS) then
    Lwt_log.notice("Got new peer @ " ^ socket_addr_to_string addr) >>
    let conn = { addr = addr; ic = ic; oc = oc} in
    PeerTbl.add p2p.connections conn
  else
    BRCMessage_channel.close_in ic >> BRCMessage_channel.close_out oc

let create ?port:(port=4000) peer_file =
  failwith "Unimplemented create"

let rec connect_to_a_peer p2p () =
  if List.length p2p.known_peers = 0 then
    Lwt.return_none
  else
    let random = Random.int (List.length p2p.known_peers) in
    let peer = List.nth p2p.known_peers random in
    let target_addr = (s_addr peer) in
    if peer_open target_addr p2p then
      Lwt.return_some (PeerTbl.find p2p.connections target_addr)
    else
      let timeout = Lwt_unix.sleep 2. >> Lwt.return_none in
      let conn_thread = connect_to_peer peer p2p in
      match%lwt Lwt.pick [timeout;conn_thread] with
      | Some peer -> Lwt.return_some peer
      | None -> connect_to_a_peer p2p ()

let list_connections p2p =
  Hashtbl.fold ( fun _ peer lst -> peer::lst) p2p.connections []

let known_peer_stream p2p =
  Lwt_stream.from(connect_to_a_peer p2p)

let connected_peer_stream p2p =
  Lwt_stream.of_list(list_connections p2p)

let peer_stream p2p =
  Lwt_stream.append (connected_peer_stream p2p) (known_peer_stream p2p)

let start_server port p2p =
  let port = Unix.(ADDR_INET (inet_addr_loopback,port)) in
  let%lwt server =
    Lwt_io.establish_server_with_client_address ~no_close:true port
      (p2p |> handle_new_peer_connection)
  in
  Lwt.return (server)

let shutdown p2p =
  List.fold_left (fun _ a -> ignore (close_peer_connection p2p a)) () (list_connections p2p);
  match p2p.server with
  | Some server -> Lwt_io.shutdown_server server
  | None -> Lwt.return_unit

let create_from_list ?port:(p=4000) (peer_list:(string * int * (Unix.tm option)) list) =
  let peers = List.map
      (fun (i,p,tm) ->
         let time = match tm with None -> Unix.gmtime 0. | Some a -> a in
         {
           ip = i;
           port = p;
           timestamp = time
         }) peer_list in
  let p2p = {
    server= None;
    connections= (PeerTbl.create 20);
    known_peers= peers;
    peer_file= "brc" ^ (string_of_int p);
  } in
  let%lwt server = start_server p p2p in
  Lwt.return {p2p with server = (Some server)}

let string_of_tm (tm:Unix.tm) =
  Unix.(Printf.sprintf "%02d:%02d:%02d %02d/%02d/%04d"
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
    (tm.tm_mon+1)
    tm.tm_mday
    (tm.tm_year + 1900))

let tm_of_string s =
  try
    Scanf.sscanf s "%02d:%02d:%02d %02d/%02d/%04d"
      (fun h m s mo d y -> Unix.(gmtime (fst (mktime
          {
            tm_sec=s; tm_min=m; tm_hour=h;
            tm_mday=d; tm_mon=mo-1; tm_year=y-1900;
            tm_wday=0; tm_yday=0; tm_isdst=false
          }))))
  with
  | Scanf.Scan_failure _
  | End_of_file
  | Unix.Unix_error (Unix.ERANGE, "mktime", _) ->
    Unix.gmtime 0.

let csv_of_peer peer =
  [peer.ip; string_of_int peer.port; string_of_tm peer.timestamp ]

let peer_of_csv s =
  {
    ip = List.nth s 0;
    port = int_of_string (List.nth s 1);
    timestamp = tm_of_string (List.nth s 2);
  }

let save_peers f p2p =
  let csv = List.map (fun p -> csv_of_peer p) p2p.known_peers in
  Csv_lwt.save f csv

let load_peers f =
  let%lwt csv = Csv_lwt.load f in
  Lwt.return @@ List.map (fun s -> peer_of_csv s) csv

let create ?port:(p=4000) s =
  let%lwt peers = load_peers s in
  let p2p = {
    server= None;
    connections= (PeerTbl.create 20);
    known_peers= peers;
    peer_file = s;
  } in
  let%lwt server = start_server p p2p in
  Lwt.return {p2p with server = (Some server)}
