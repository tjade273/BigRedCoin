open Lwt
exception FailedToConnect of string

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
module BRCMessage_channel = struct 

  type input = Lwt_io.input Lwt_io.channel

  type output = Lwt_io.output Lwt_io.channel

  let write oc msg = 
    let encoder = Pbrt.Encoder.create() in
    Message_pb.encode_message msg encoder;
    let buf = Pbrt.Encoder.to_bytes encoder in
    Lwt_io.write_from oc buf 0 (Bytes.length buf) >|= ignore

  (*let read ic =  
    let%lwt (buf_len,buf) = read_raw_msg ic in
    Lwt.return (Message_pb.decode_message (Pbrt.Decoder.of_bytes buf))*)
end

type peer_data = {
  ic: BRCMessage_channel.input;
  oc: BRCMessage_channel.output;
  last_seen: float
}

let make_peer_data input output last_time = 
  {ic=input; oc=output; last_seen=last_time}

module PeerTbl = struct
  include Hashtbl

  let remove tbl addr =
    match find_opt tbl addr with
    | Some peer_data -> remove tbl addr; 
      Lwt_io.close peer_data.ic >> Lwt_io.close peer_data.oc
    | None -> Lwt.return_unit

  let add tbl addr peer_data =
    remove tbl addr >>
    Lwt.return @@ add tbl addr peer_data
end

type t = {
  neighbor_peers:(string,peer_data) PeerTbl.t;
  known_peers:(string*int) list
}
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
  let%lwt (ic, oc)  = Lwt_io.open_connection peer_addr in
  Lwt.return (socket_addr_to_string peer_addr,ic,oc)

let connect_to_peer ip port =
  let addr = Unix.(ADDR_INET (Unix.inet_addr_of_string ip, port)) in
  try initiate_connection addr with
  | _ -> Lwt_log.notice ("Failed to connect to:" ^ ip ^ ":" ^ (string_of_int port)) >>
    fail_with "Failed"

let send_raw bytes size oc = 
  Lwt_io.write_from_exactly oc bytes 0 size

let broadcast (msg:Message_types.message) (p2p:t) = 
  PeerTbl.fold (fun _ peer_data acc -> 
      let bytes = encode_message msg in
      (send_raw (bytes) (Bytes.length bytes) peer_data.oc)<&>acc) 
    p2p.neighbor_peers Lwt.return_unit

let handle_new_peer_connection p2p addr (input,out) =
  Lwt_log.notice("Got new peer @ " ^ socket_addr_to_string addr) >>
  let peer_data = make_peer_data input out (Unix.time()) in
  let sock_addr = socket_addr_to_string addr in
  PeerTbl.add p2p sock_addr peer_data 

let create ?port:(port=4000) peer_file = failwith "Unimplemented create"

let peer_stream p2p = failwith "Unimplemented peer stream"

let create_from_list (peer_list:(string*int) list) = 
  {neighbor_peers=(PeerTbl.create 20);known_peers=peer_list} 

let connect_to_known_peers p2p = 
  let cons = List.fold_left(fun acc (addr,port) -> 
    (connect_to_peer addr port)::acc  
  ) [] p2p.known_peers in
  Lwt_log.notice (string_of_int (List.length cons)) >>
  match%lwt Lwt.choose (cons) with 
  | (addr,ic,oc) -> Lwt_log.notice ("Connected to peer: " ^ addr) 
      >> PeerTbl.add p2p.neighbor_peers addr (make_peer_data ic oc 0.)
  
let start_server ?port:(p=4444) p2p  =
  let port = Unix.(ADDR_INET (inet_addr_loopback,p)) in
  let%lwt server =
    Lwt_io.establish_server_with_client_address ~no_close:true port 
      (p2p.neighbor_peers |> handle_new_peer_connection)
  in
    Lwt.return (print_endline ("Server started on " ^ 
      socket_addr_to_string port ^"!"))

let num_of_peers p2p =
  PeerTbl.length p2p.neighbor_peers

let read_raw_msg ic = 
  flush stderr;
  let buf = Bytes.create 2048 in
  let%lwt read = Lwt_io.read_into ic buf 0 2048 in
  Lwt.return (read,buf)

let rec read_message_from_a_peer p2p () =
  Lwt_log.notice ("Reading Message from a peer") >>
  let timeout = Lwt_unix.sleep 1.0 >> Lwt.return (0,Bytes.empty) in
  let threads =
    PeerTbl.fold (fun addr peer_data acc -> 
        (read_raw_msg peer_data.ic)::acc) p2p.neighbor_peers []
  in
  match%lwt Lwt.pick (timeout::threads) with
  | (0,_) -> read_message_from_a_peer p2p ()
  | (size,bytes) -> Lwt.return_some (decode_message bytes)

let in_message_stream (p2p:t) : Message_types.message Lwt_stream.t = 
  Lwt_stream.from ((p2p |> read_message_from_a_peer))
