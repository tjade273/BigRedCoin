open Lwt
open Message_types
open P2p

let suites : Test.suite list = []


open Test

let messaging_tests = suite "messaging tests" [

  test "test_test" begin fun () ->
    Lwt.return true
  end;

  test "open_server" begin fun () ->
    try let%lwt p2p = P2p.create_from_list ~port:4444 [("127.0.0.1",4445,None)]
    in P2p.shutdown p2p >> Lwt.return true
    with
    | _ -> Lwt.return false
  end;

  test "simple_message" begin fun () ->
    let msg = {method_=(Message_types.Get);get=None;post=None} in
    let%lwt node_a = P2p.create_from_list ~port:4444 [("127.0.0.1",4445,None)] in
    let%lwt node_b = P2p.create_from_list ~port:4445 [("127.0.0.1",4444,None)] in
    P2p.broadcast msg node_b >>
    let res = Lwt_stream.get (P2p.peer_stream node_a) in
    match%lwt res with
    | Some peer ->
      (match%lwt BRCMessage_channel.read (BRCPeer.ic peer) with
       | Some msg -> P2p.shutdown node_a >> P2p.shutdown node_b >> Lwt.return true
       | None -> P2p.shutdown node_a >> P2p.shutdown node_b >> Lwt.return false)
    | None -> P2p.shutdown node_a >> P2p.shutdown node_b >> Lwt.return false;
  end;

  test "peer_file" begin fun () ->
    let msg = {method_=(Message_types.Get);get=None;post=None} in
    let%lwt node_a = P2p.create ~port:4444 "node_a" in
    let%lwt node_b = P2p.create ~port:4445 "node_b" in
    P2p.broadcast msg node_b >>
    let res = Lwt_stream.get (P2p.peer_stream node_a) in
    match%lwt res with
    | Some peer ->
      (match%lwt BRCMessage_channel.read (BRCPeer.ic peer) with
       | Some msg -> P2p.shutdown node_a >> P2p.shutdown node_b >> Lwt.return true
       | None -> P2p.shutdown node_a >> P2p.shutdown node_b >> Lwt.return false)
    | None -> P2p.shutdown node_a >> P2p.shutdown node_b >> Lwt.return false;
  end;
]

let suites = suites @ [messaging_tests]

let () = Test.run "all_test" suites
