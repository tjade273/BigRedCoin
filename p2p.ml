open Lwt
open Csv_lwt
open Message_types

exception FailedToConnect of string

let data_preamble = {
  method_= Manage;
  get=None;
  post=None;
  manage = 
    Some {
      manage_type= Data_p;
      peers = [];
    }
}

let ping_msg = {
  method_=Manage;
  get=None;
  post=None;
  manage=Some(
      {
        manage_type = Ping;
        peers = [];
      }
    ) 
}

let c_MAX_CONNECTIONS = 1024


module type Message_channel = sig
  type input
  type output
  val write : output -> Message_types.message -> int Lwt.t
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
    let%lwt bytes_written = Lwt_io.write_from oc buf 0 (Bytes.length buf) in
    Lwt.return bytes_written

  let read_raw_msg ic =
    let timeout = Lwt_unix.sleep 1. >> Lwt.return (0,Bytes.empty) in
    let buf = Bytes.create 2048 in
    let read =
      let%lwt sz = Lwt_io.read_into ic buf 0 2048 in Lwt.return(sz,buf) in
    Lwt.pick[timeout;read]

  let read ic =
    try 
      match%lwt read_raw_msg ic with
      | (0,_) -> Lwt.return_none
      | (_,buf) ->
        Lwt.return_some (Message_pb.decode_message (Pbrt.Decoder.of_bytes buf))
    with 
    | _-> Lwt_log.notice ("Failed read incomming message") >> Lwt.return_none

  let close_in ic =
    try 
      Lwt_log.notice "Closing input channel" >>
      Lwt_io.close ic
    with 
    | _ -> Lwt_log.notice ("Failed to close input chanel")

  let close_out oc =
    try 
      Lwt_log.notice "Closing ouput channel" >>
      Lwt_io.close oc
    with 
    | _ -> Lwt_log.notice ("Failed to close input chanel")
end

module type BRCPeer_t = sig
  type peer_connection
  type peer
  val null_peer : unit -> peer
  val new_peer : string -> int -> peer
  val (<=>) : peer -> peer -> bool
  val addr : peer -> Unix.sockaddr
  val s_addr : peer -> string
  val str : peer_connection -> string
  val ic : peer_connection -> BRCMessage_channel.input
  val oc : peer_connection -> BRCMessage_channel.output
end

(* The type of a socket connection to an address, with the input and output
 * channels associated with the connection. *)
type connection = {
  addr: Unix.sockaddr;
  ic: BRCMessage_channel.input;
  oc: BRCMessage_channel.output;
}

module BRCPeer : BRCPeer_t
  with type peer = Message_types.peer and
  type peer_connection = connection
= struct
  type peer_connection = connection
  type peer = Message_types.peer
  let null_peer () = { address = ""; port = 0; last_seen = 0;}
  let new_peer addr port = { address = addr; port = port; last_seen = 0;}
  let (<=>) p1 p2 = p1.address = p2.address && p1.port = p2.port
  let addr p = Unix.(ADDR_INET (inet_addr_of_string p.address, p.port))
  let s_addr p = p.address ^ ":" ^ (string_of_int p.port)
  let socket_addr_to_string addr =
    match addr with
    | Lwt_unix.ADDR_UNIX addr -> addr
    | Lwt_unix.ADDR_INET (addr,port) ->
      (Unix.string_of_inet_addr addr) ^ ":" ^ (string_of_int port)
  let str conn = socket_addr_to_string conn.addr
  let ic conn = conn.ic
  let oc conn = conn.oc
end

open BRCPeer

module PeerList = struct
  include Array

  (* The type of the item (peer) in the list. *)
  type item = peer

  (* The type of the list. *)
  type t = item array

  (* [similar p1 p2] is true if the core structure of [p1] and [p2] is
   * equal, false otherwise. The comparison method is used by [find] and
   * [update]. *)
  let similar p1 p2 =
    p1 <=> p2

  (* [find p lst] is the index of the first item that is similar to [p] in the
   * given Peer List [lst] if such an item exists in the list, None otherwise. *)
  let find p lst =
    let (found, i) = fold_left
        (fun (found, index) a ->
           if (similar a p) then (true, index)
           else (false, index+1)) (false,0) lst in
    if found then Some i else None

  (* [remove i lst] is the given list [lst] with the item at position [i]
   * removed. *)
  let remove i lst =
    let new_arr = make ((Array.length lst)-1) (null_peer ()) in
    iteri (fun pos item ->
        if pos < i then new_arr.(pos) <- lst.(pos)
        else if pos > i then new_arr.(pos-1) <- lst.(pos)
        else ()) lst;
    new_arr

  (* [update i p lst] is the given list [lst] with the item at position [i]
   * replaced by the item [p]. *)
  let update i item lst =
    lst.(i) <- item; lst

  (* [modify p lst] is the given list [lst] with the first item that is similar
   * to [p] replaced by the given version of [p]. *)
  let modify p lst =
    iteri (fun i a -> if (similar a p) then lst.(i) <- p else ()) lst; lst

  (* [append p lst] is the given list [lst] with the item [p] added to it. If
   * a similar item already exists, the item is modified. *)
  let append p lst =
    if exists (fun a -> similar a p) lst then modify p lst
    else Array.append [|p|] lst

  let filter lst f  =
    let as_lst = Array.to_list lst in 
    let filtered = List.filter f as_lst in 
    Array.of_list filtered  

end

module ConnTbl = struct
  include Hashtbl

  (* [remove tbl addr] removes the value associated with key [addr] from the
   * given table [tbl], while also managing all the resources associated with
   * value. leaves [tbl] unmodified if [addr] is not a key in the table. *)
  let remove tbl addr =
    match find_opt tbl addr with
    | Some peer -> remove tbl addr;
      (try 
         BRCMessage_channel.close_in peer.ic >>
         BRCMessage_channel.close_out peer.oc with 
      | _ -> Lwt_log.notice ("Failed to close: " ^ addr))
    | None -> Lwt.return_unit

  (* [add tbl peer] adds [peer] to the given table [tbl] using a string
   * represenation of [peer] as the key. If the [peer] is already in
   * the table, the [peer] is removed and replced by the given copy. *)
  let add tbl peer =
    remove tbl (str peer) >>
    Lwt.return @@ add tbl (str peer) peer
end

type t = {
  connections:(string,peer_connection) ConnTbl.t;
  data_connections:(string,peer_connection) ConnTbl.t;
  handled_connections:(string,peer_connection) Hashtbl.t;
  mutable known_peers: PeerList.t;
  server:Lwt_io.server option;
  mutable port:int;
  peer_file:string}

let server_port p2p =
  p2p.port

(* [id p2p] is the string id of the [p2p] node, used for logging. *)
let id p2p =
  string_of_int p2p.port

(* [is_conn_open addr p2p] is true iff there is an open connection to the given
 * address [addr] in the given [p2p] node *)
let is_conn_open addr p2p =
  ConnTbl.mem p2p.connections addr

(* [get_connection addr p2p] is the peer connection with the given address
 * [addr] in the given [p2p] node.*)
let get_connection addr p2p =
  ConnTbl.find p2p.connections addr

(* [remove_handle_connection conn p2p] removes the given peer_connection [conn]
 * from the table of handled connection in the given [p2p] node. *)
let remove_handle_connection conn p2p =
  Hashtbl.remove p2p.handled_connections (str conn);
  Lwt.return_unit
let close_peer_connection p2p (peer:peer_connection) =
  ConnTbl.remove p2p.connections (str peer)
(* [handle f p2p conn] adds a connection tow the [handled_connections table]
 * and handles the conneuction using [f]. Connections currently being handled 
 * will not be exposed via the peer stream. *)
let handle f p2p conn  =
  Hashtbl.add p2p.handled_connections (str conn) conn;
  let%lwt (close,res) = f conn in
  if close then close_peer_connection p2p conn >> res
  else remove_handle_connection conn p2p >> res

(* [@<>] operator for [handle]]*)
let (@<>) (p2p,conn) f =
  handle f p2p conn

(* [add_new_peer (addr,port) p2p] adds a peer with given [addr] and [port] to
 * the list of known peers of the [p2p] node *)
let add_new_peer (addr,port) p2p =
  p2p.known_peers <- PeerList.append (new_peer addr port) p2p.known_peers

(* [remove_known_peer p p2p] removes the peer [p] from the list of known peers
 * for the [p2p] node. *)
let remove_known_peer p p2p =
  match PeerList.find p p2p.known_peers with
  | Some i -> p2p.known_peers <- PeerList.remove i p2p.known_peers
  | None -> ()

let peer_open inet p2p =
  ConnTbl.mem p2p.connections inet

let get_connected_peer addr p2p =
  ConnTbl.find p2p.connections addr
(* [string_of_tm tm] is the string representation of the time [tm]. *)
let string_of_tm tm =
  Unix.(Printf.sprintf "%02d:%02d:%02d %02d/%02d/%04d"
          tm.tm_hour
          tm.tm_min
          tm.tm_sec
          (tm.tm_mon+1)
          tm.tm_mday
          (tm.tm_year + 1900))

(* [tm_of_string s] is the time parsed from [s]. If there is an error during
 * the parsing, the time is 00:00:00 GMT, Jan. 1, 1970. *)
let tm_of_string s =
  try
    Scanf.sscanf s "%02d:%02d:%02d %02d/%02d/%04d"
      (fun h m s mo d y ->
         Unix.(snd (mktime{
             tm_sec=s; tm_min=m; tm_hour=h;
             tm_mday=d; tm_mon=mo-1; tm_year=y-1900;
             tm_wday=0; tm_yday=0; tm_isdst=false
           })))
  with
  | Scanf.Scan_failure _
  | End_of_file
  | Unix.Unix_error (Unix.ERANGE, "mktime", _) ->
    Unix.localtime 0.

(* [csv_of_peer peer] is the csv representation of the given [peer]. *)
let csv_of_peer peer =
  [
    peer.address;
    string_of_int peer.port;
    string_of_tm (Unix.localtime (float_of_int peer.last_seen))
  ]

(* [peer_of_csv s] is the peer parsed from the csv representation [s] *)
let peer_of_csv s =
  let (time,_) = Unix.mktime (tm_of_string (List.nth s 2)) in
  {
    address = List.nth s 0;
    port = int_of_string (List.nth s 1);
    last_seen = int_of_float time;
  }

(* [peer_cmp p1 p2] is a comparison between the peers [p1] and [p2]. It returns:
 * -1, if [p1] was last seen after [p2],
 * 0, if [p1] and [p2] were seen at the same time, 1 otherwise. *)
let peer_cmp p1 p2 =
  if p1.last_seen > p2.last_seen then -1
  else if p1.last_seen = p2.last_seen then 0
  else 1

(* [save_peers f p2p] saves the list of know peers for the [p2p] node, in the
 * file with the name [f], but printing it as csv (comma-separated values). *)
let save_peers f p2p =
  PeerList.sort peer_cmp p2p.known_peers;
  let csv = PeerList.map (fun p -> csv_of_peer p) p2p.known_peers in
  Csv_lwt.save f (PeerList.to_list csv)

(* [load_peers f] is the list of peers parsed from the file with the name [f]. *)
let load_peers f =
  let%lwt csv = Csv_lwt.load f in
  Lwt.return @@ PeerList.of_list (List.map (fun s -> peer_of_csv s) csv)

(* [socket_addr_to_string addr] is the string representation of the socket
 * address [addr]. *)
let socket_addr_to_string addr =
  match addr with
  | Lwt_unix.ADDR_UNIX addr -> addr
  | Lwt_unix.ADDR_INET (addr,port) ->
    (Unix.string_of_inet_addr addr) ^ ":" ^ (string_of_int port)

(* [decode_message bytes] is the message, decoded from the given bytes *)
let decode_message bytes =
  (Message_pb.decode_message (Pbrt.Decoder.of_bytes bytes))

(* [encode_message msg] is the byte-encoded form of the given message [msg] *)
let encode_message msg =
  let encoder = Pbrt.Encoder.create() in
  Message_pb.encode_message msg encoder;
  Pbrt.Encoder.to_bytes encoder
(* [initiate_connection peer_addr p2p] attemps to iniate a connection with
 * a peer at address [peer_addr]. Returns [Some BRCPeer.t] if connection
 * establishes successfully. *)
let initiate_connection peer_addr p2p =
  Lwt_log.notice ((string_of_int p2p.port)^": Attempting to initiate connection: " ^ socket_addr_to_string peer_addr) >>
  Lwt.catch (
    fun () -> let%lwt (ic, oc)  = Lwt_io.open_connection peer_addr in
      Lwt.return_some {addr=peer_addr;ic=ic;oc=oc})
    (fun e -> Lwt.return_none)
(* [read_for_manage_ping conn] reads for an initial connection ping. *)
let rec read_for_manage_ping conn = 
  match%lwt BRCMessage_channel.read (ic conn) with 
  | Some msg -> 
    (match msg.method_ with
     | Manage ->
       (match msg.manage with 
        | Some manage_data when manage_data.manage_type = Ping ->
          Lwt.return_some true 
        | _ -> 
          Lwt.return_none)
     | _ -> Lwt_log.notice ("Not a manage frame") >> Lwt.return_none
    )
  | None -> Lwt.return_none

(* [send_till_success conn msg] sends the given message [msg] through the given
 * peer connection [conn] and keeps trying until successful. *)
let rec send_till_success conn msg =
  let%lwt bytes_sent = BRCMessage_channel.write conn.oc msg in
  if bytes_sent = 0 then
    send_till_success conn msg
  else
    Lwt.return_some ()
(*[get_connection_ping conn] wraps [read_for_manage_ping] in a timeout *)
let get_connection_ping conn = 
  let timeout = Lwt_unix.timeout 2. >> Lwt.return None in
  (match%lwt Lwt.pick [timeout;(read_for_manage_ping conn)] with 
   | Some _ -> Lwt.return_some conn
   | None -> Lwt.return_none 
  )
(* [connect_to_peer peer p2p] is a peer connection to the given peer or None
 * if no connection was found or failed to be established. If a connection was
 * found, it is added to the table of opened connections in the [p2p] node,
 * and the last seen for the [peer] is updated in the node's list of known
 * peers. *)
let connect_to_peer peer p2p =
  let target = (s_addr peer) in
  if (peer_open target p2p) then
    let conn = (get_connected_peer target p2p) in
    Lwt.return_some conn
  else
    let addr = Unix.(ADDR_INET (Unix.inet_addr_of_string peer.address, peer.port)) in
    match%lwt (initiate_connection addr p2p) with
    | Some conn -> 
      (match%lwt get_connection_ping conn with 
       | Some _ ->  ConnTbl.add p2p.connections conn >> Lwt.return_some conn
       | None -> Lwt_log.notice ("Failed to recieve connection ping from: " ^ target)
         >> Lwt.return_none 
      )
    | None -> Lwt.return_none

(* [connect_and_send peer msg p2p] connects to [peer] and sends the given
 * message [msg] through the [p2p] node. *)
let connect_and_send peer msg p2p =
  match%lwt connect_to_peer peer p2p with
  | Some conn -> 
    let timeout = Lwt_unix.sleep 2. >> Lwt.return None in      
    (match%lwt Lwt.pick [timeout;(send_till_success conn msg)] with
     | Some _ -> 
       (match msg.method_ with 
        | Get -> Lwt_log.notice ("Wrote get broadcast to: " ^ (str conn))
        | Post -> Lwt_log.notice ("Wrote post broadcast to: " ^ (str conn))
        | Manage -> Lwt_unix.sleep 0.0 (*TODO: Find a better way to do this*) 
          >> Lwt_log.notice ("Wrote manage broadcast to: " ^ (str conn)))
     | None -> Lwt_log.notice ("Failed to broadcast to: " ^ (str conn))
    )
  | None -> Lwt.return_unit

let broadcast (msg:Message_types.message) (p2p:t) =
  Array.fold_left (fun acc peer ->
      (connect_and_send peer msg p2p)<&>acc) Lwt.return_unit p2p.known_peers
(* [read_for_time conn time] is the message read from the given connection. it
 * times out aftom [time] seconds, in which case, None is returned. *)
let read_for_time conn time =
  let timeout = Lwt_unix.sleep time >> Lwt.return None in
  let read = BRCMessage_channel.read conn in
  match%lwt Lwt.pick[read;timeout] with
  | Some msg -> Lwt.return_some msg
  | None -> Lwt.return_none
(* [handle_new_peer_connection p2p addr (ic,oc)] handles new incoming connections
 * to the server of the [p2p] node. It adds the connection to the table of
 * open connections if there is space for more connections, otherwise closes the
 * connection. *)
let handle_new_peer_connection p2p addr (ic,oc) =
  if (Hashtbl.length p2p.connections < c_MAX_CONNECTIONS) then
    Lwt_log.notice((string_of_int p2p.port) ^ ": Got new peer @ " ^ socket_addr_to_string addr) >>
    let conn = { addr = addr; ic = ic; oc = oc} in
    let timeout = Lwt_unix.sleep 2.0 >> Lwt.return None in    
    (match%lwt Lwt.pick [timeout;(send_till_success conn ping_msg)] with
     | Some _ -> ConnTbl.add p2p.connections conn >>
       (match%lwt read_for_time (BRCPeer.ic conn) 2. with 
        | Some msg -> 
          (match msg.method_ with 
           | Manage -> 
             (match msg.manage with 
              | Some manage_data -> 
                (match manage_data.manage_type with 
                 | Peer_p -> failwith ("Handle peer stream")
                 | Data_p -> ConnTbl.add p2p.data_connections conn >> 
                   Lwt_log.notice("Recieved data preamble.")
                 | _ -> Lwt_log.notice("First message was not a preamble.")
                )
              | None -> Lwt_log.notice("No management data")
             )
           | _ -> Lwt_log.notice("First message was not a management frame.")
          )
        | None -> Lwt_log.notice("Failed to recieve a preamble.")
       )
     | None -> Lwt_log.notice("Failed to send ping.")
    )
  else
    BRCMessage_channel.close_in ic >> BRCMessage_channel.close_out oc

(* [connect_to_a_peer p2p peers ()] is a peer connection to a random peer
 * from the list of given [peers] of the [p2p] node, or None if an empty List
 * is given. *)
let rec connect_to_a_peer_for_data p2p ?peers:(peers=p2p.known_peers) () =
  if PeerList.length peers = 0 then
    Lwt.return_none
  else
    let random = Random.int (PeerList.length peers) in
    let peer = peers.(random) in
    let target_addr = s_addr peer in
    if (ConnTbl.mem p2p.connections target_addr) then
      let peer_connection = ConnTbl.find p2p.connections target_addr in
      Lwt.return_some peer_connection
    else
      let timeout = Lwt_unix.sleep 2. >> Lwt.return_none in
      let conn_thread = connect_to_peer peer p2p in
      match%lwt Lwt.pick [timeout;conn_thread] with
      | Some peer -> (send_till_success peer data_preamble) >|= ignore
        >> Lwt.return_some peer
      | None ->
        let good_peer_lst = PeerList.filter peers (fun p -> p <> peer)  in
        connect_to_a_peer_for_data p2p ~peers:good_peer_lst ()




(* [initiate_connection addr] is a peer connection to the given address [addr],
 * if successfully opened, None otherwise *)
let initiate_connection addr : peer_connection option Lwt.t =
  Lwt.catch (
    fun () -> let%lwt (ic, oc)  = Lwt_io.open_connection addr in
      Lwt.return_some {addr=addr;ic=ic;oc=oc})
    (fun e -> Lwt.return_none)



(* [tbl_to_list tbl] is a list of all the values in the given table [tbl]. *)
let tbl_to_list tbl =
  Hashtbl.fold ( fun _ peer lst -> peer::lst) tbl []

(* [known_peer_stream p2p] is a stream of peer connections to random known peers of
 * the [p2p] node. *)
let known_peer_stream p2p =
  Lwt_stream.from(connect_to_a_peer_for_data p2p)

(* [unhandled_connected_peer_stream p2p] is a stream of peer connections that
 * are already opened in the [p2p] node and are not being handled currently
 * elsewhere. *)
let unhandled_connected_peer_stream p2p =
  let unhandled = Hashtbl.copy p2p.data_connections in
  Hashtbl.filter_map_inplace(
    fun addr peer ->
      if (Hashtbl.mem p2p.handled_connections addr) then None
      else Some peer
  ) unhandled;
  Lwt_stream.of_list(tbl_to_list unhandled)

let peer_stream p2p =
  Lwt_stream.append (unhandled_connected_peer_stream p2p) (known_peer_stream p2p) 

(* [start_server p2p] is a server on the port of the given [p2p] node. *)
let start_server p2p =
  let port = Unix.(ADDR_INET (inet_addr_loopback, server_port p2p)) in
  let%lwt server =
    Lwt_io.establish_server_with_client_address ~no_close:true port
      (p2p |> handle_new_peer_connection)
  in
  Lwt.return (server)
let create_from_list ?port:(p=4000) peer_list =
  let peers = Array.of_list (List.map
                               (fun (i,p,tm) ->
                                  let time = match tm with None -> 0. | Some a -> (fst (Unix.mktime a)) in
                                  {
                                    address = i;
                                    port = p;
                                    last_seen = int_of_float time
                                  })
                               peer_list) in
  let p2p = {
    server= None;
    port = p;
    handled_connections = Hashtbl.create 20;
    connections= (ConnTbl.create 20);
    known_peers= peers;
    data_connections = (ConnTbl.create 20);
    peer_file = "nodes/brc" ^ (string_of_int p) ^ ".peers";
  } in
  let%lwt server = start_server p2p in
  Lwt.return {p2p with server = (Some server);port=p}

let create ?port:(p=4000) f =
  let%lwt peers = load_peers f in
  let p2p = {
    server= None;
    port = p;
    handled_connections = Hashtbl.create 20;
    connections= (ConnTbl.create 20);
    data_connections = (ConnTbl.create 20);
    known_peers= peers;
    peer_file = f;
  } in
  let%lwt server = start_server p2p in
  Lwt.return {p2p with server = (Some server)}

let shutdown p2p =
  Hashtbl.fold (fun _ a _ ->
      ignore (close_peer_connection p2p a)) (p2p.connections) ();
  save_peers p2p.peer_file p2p >>
  match p2p.server with
  | Some server -> Lwt_io.shutdown_server server
  | None -> Lwt.return_unit
