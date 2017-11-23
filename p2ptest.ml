open Lwt
open Message_types
open P2p

let suites : Test.suite list = []

open Test

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

let peer_preable = 
  {method_=(Message_types.Manage);
   get=None;
   post=None;
   manage=
     Some {
       manage_type = Peer_p;
       peers = [];
     }
  }

let simple_data_msg = 
  {method_=(Message_types.Get);
   get=None;
   post=None;
   manage=None;
  }

let close_all (node_lst:P2p.t array) = 
  Array.fold_left (fun acc node -> 
       (P2p.shutdown node)<&>acc) Lwt.return_unit node_lst 

let rec create_n_linked_nodes ?start_port:(start_port=4000) n =
  let rec create_nodes_rec p lst =
    if p > start_port + n then  
      Lwt.return (Array.of_list lst)
    else
      let%lwt new_node =  P2p.create_from_list ~port:p 
          ["127.0.0.1",(p-1),None] in
      create_nodes_rec (p+1) (lst @ [new_node])
  in
  create_nodes_rec start_port []

let messaging_tests = suite "messaging tests" [

    test "test_test" begin fun () ->
      Lwt.return true
    end;

    test "open_server" begin fun () ->
      try let%lwt p2p = P2p.create_from_list ~port:4444 
              [("127.0.0.1",4445,None)]
        in  P2p.shutdown p2p >> Lwt.return true
      with 
      | _ -> Lwt.return false
    end;

    test "simple_message_peer_list" begin fun () -> 
      let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
      P2p.broadcast data_preamble nodes.(1) 
      >> P2p.broadcast simple_data_msg nodes.(1) >>
      let res = Lwt_stream.get (P2p.peer_stream nodes.(0)) in
      let%lwt check_message = match%lwt res with 
        | Some peer ->  (nodes.(0),peer) @<> (fun peer ->
            (match%lwt BRCMessage_channel.read (BRCPeer.ic peer) with
             | Some msg -> Lwt.return (true,Lwt.return true)
             | None -> Lwt.return (true,Lwt.return false))) 
        |None -> Lwt.return false
      in close_all nodes >> Lwt.return check_message
    end;

    test "simple_message_peer_file" begin fun () -> 
      let%lwt node_a = P2p.create ~port:4444 "node_a.peers" in
      let%lwt node_b = P2p.create ~port:4445 "node_b.peers" in
      P2p.broadcast data_preamble node_b  >> 
      P2p.broadcast simple_data_msg node_b >>
      let peer = Lwt_stream.get (P2p.peer_stream node_a) in
      let%lwt check_message = 
      match%lwt peer with 
        | Some peer -> Lwt_log.notice (BRCPeer.str peer) >>
            (node_a,peer) @<> (fun peer ->
            (match%lwt BRCMessage_channel.read (BRCPeer.ic peer) with
             | Some msg -> Lwt.return (true,Lwt.return true)
             | None -> Lwt.return (true,Lwt.return false))) 
        |None -> Lwt_unix.sleep 2. >> Lwt.return_false
      in close_all [|node_a;node_b|] >> Lwt.return check_message
    end;

    test "test_connect_random" begin fun () -> 
      let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
      let peer_opt = Lwt_stream.get (P2p.peer_stream nodes.(1)) in
      let%lwt check_connection = 
        match%lwt peer_opt with 
        | Some peer -> Lwt.return true 
        | None -> Lwt.return false;
      in 
        Lwt_unix.sleep 0.0 >> close_all nodes >> Lwt.return check_connection
    end;

  test "test_random_connection_failed" begin fun () -> 
      let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
      let peer_opt = Lwt_stream.get (P2p.peer_stream nodes.(0)) in
      let%lwt check_connection = 
        match%lwt peer_opt with 
        | Some peer -> Lwt.return false 
        | None -> Lwt.return true;
      in close_all nodes >> Lwt.return check_connection
    end
  ]

let suites = suites @ [messaging_tests]

let () = Test.run "all_test" suites
