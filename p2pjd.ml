open Lwt
type channel = (Lwt_io.input_channel * Lwt_io.output_channel)

module PeerTbl = struct
  include Hashtbl
  let remove tbl x =
    match find_opt tbl x with
    | Some (ic, oc) -> remove tbl x; Lwt_io.close ic >> Lwt_io.close oc
    | None -> Lwt.return_unit
  let add tbl x y =
    remove tbl x >>
    Lwt.return @@ add tbl x y
end
let peer_table = PeerTbl.create 25

let send_pingpong p oc =
  let encoder = Pbrt.Encoder.create () in
  Peer_pb.encode_ping {pingpong = p} encoder;
  let buf = Pbrt.Encoder.to_bytes encoder in
  Lwt_io.write_from oc buf 0 (Bytes.length buf)
  >|= ignore

let read_pingpong ic =
  let buf = Bytes.create 100 in
  let%lwt inlen = Lwt_io.read_into ic buf 0 100 in
  Lwt.return (Peer_pb.decode_ping (Pbrt.Decoder.of_bytes buf)).pingpong

let handle_incoming peer_addr (ic, oc) =
  Lwt_log.notice "Got connection" >>
  match%lwt read_pingpong ic with
  | Ping -> (send_pingpong Pong oc) >> Lwt_log.notice "Got Ping!"
    >> PeerTbl.add peer_table peer_addr (ic, oc)
  | Pong-> Lwt_log.notice "Got Pong!"
  | exception Protobuf.Decoder.Failure _ -> Lwt_log.notice "Failure"

let initiate_connection peer_addr =
  let%lwt (ic, oc)  = Lwt_io.open_connection peer_addr in
  send_pingpong Ping oc >>
  let%lwt response = read_pingpong ic in
  if response = Pong
  then PeerTbl.add peer_table peer_addr (ic,oc) >> Lwt_log.notice "Got Pong!"
  else Lwt_io.close ic >> Lwt_io.close oc

let start_server p=
  let port = Unix.(ADDR_INET (inet_addr_any, int_of_string p)) in
  let%lwt server =
    Lwt_io.establish_server_with_client_address ~no_close:true port handle_incoming
  in
  Lwt.return (print_endline "Server started!")

let connect_to_peer port =
  let addr = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  initiate_connection addr

let read_peer x (ic, oc) =
  match%lwt Lwt_io.read_line_opt ic with
  | Some msg -> Lwt.return_some msg
  | None -> PeerTbl.remove peer_table x >> (fst @@  Lwt.wait ())

let rec read_some_peer () =
  Lwt_log.notice "Read Peer" >>
  let timeout = Lwt_unix.sleep 1.0 >> Lwt.return_none in
  let threads =
    PeerTbl.fold (fun x chan acc -> (read_peer x chan)::acc) peer_table []
  in
  match%lwt Lwt.pick (timeout::threads) with
  | Some msg -> Lwt.return_some msg
  | None -> read_some_peer ()

let stdin_stream : string Lwt_stream.t = Lwt_stream.from (fun () -> Lwt_io.(read_line_opt stdin))
let peer_stream : string Lwt_stream.t = Lwt_stream.from (read_some_peer)
let message_stream : string Lwt_stream.t = Lwt_stream.choose [stdin_stream; peer_stream]

let blast_msg msg =
  Lwt_log.notice msg >>
  PeerTbl.fold (fun _ (_,oc) acc -> (Lwt_io.write_line oc msg)<&>acc)
    peer_table Lwt.return_unit

let relay_messages  =
  Lwt_log.notice "Stream started" >>
  Lwt_stream.iter_p blast_msg message_stream >> Lwt_log.notice "Stream ended"

let () =
  let p1 =
    if Array.length Sys.argv > 2 then
      let peer_port = int_of_string Sys.argv.(2) in
      connect_to_peer peer_port
    else Lwt.return_unit
  in

  let main = start_server Sys.argv.(1) >> p1 >> relay_messages in Lwt_main.run (main)