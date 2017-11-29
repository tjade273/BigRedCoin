open Lwt
open Message_types
open P2p
open Lwt_test

let suites : Lwt_test.suite list = []


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

let test name f = 
  Lwt_test.test name (fun () -> 
      Lwt_log.notice("Starting Test: "^ name) >>
      let%lwt result = f () in
      Lwt_log.notice("Finished Test: "^name^ "\n") 
      >>  Lwt_unix.sleep 0.001 >>Lwt.return result    
    )
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

(*Checks to see if any data message is avaiable for reading from the first
  connection in the peer stream*)
let message_check_thread node = 
  let%lwt peer = Lwt_stream.get( P2p.peer_stream node) in
  match peer with 
  | Some peer -> 
    (node,peer) @<> (fun peer ->
        (match%lwt BRCMessage_channel.read (BRCPeer.ic peer) with
         | Some msg ->  Lwt.return (true,Lwt.return true)
         | None -> Lwt_log.notice "No message" >> Lwt.return (true,Lwt.return false))) 
  |None -> Lwt_log.notice "No peer" >> Lwt.return_false 

let close_all (node_lst:P2p.t array) = 
  Array.fold_left (fun acc node -> 
      (P2p.shutdown ~save:false node)<&>acc) Lwt.return_unit node_lst 

let rec create_n_linked_nodes ?start_port:(start_port=4000) n =
  let rec create_nodes_rec p lst =
    if p >= start_port + n then
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
      try let%lwt p2p = P2p.create_from_list ~port:4449
              [("127.0.0.1",4450,None)]
        in close_all [|p2p|] >> Lwt.return true
      with
      | _ -> Lwt.return false
    end;

    test "test_random_connection_failed" begin fun () -> 
      let%lwt nodes = create_n_linked_nodes ~start_port:4000 1 in
      let peer_opt = Lwt_stream.get (P2p.peer_stream nodes.(0)) in
      let%lwt check_connection =
        match%lwt peer_opt with
        | Some peer -> Lwt.return false
        | None -> Lwt.return true;
      in close_all nodes >> Lwt.return check_connection
    end;

    test "test_random_connection_passed" begin fun () -> 
    let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
    let peer_opt = Lwt_stream.get (P2p.peer_stream nodes.(1)) in
    let%lwt check_connection =
      match%lwt peer_opt with
      | Some peer -> Lwt.return true
      | None -> Lwt.return false;
    in close_all nodes >> Lwt.return check_connection
  end;

   test "simple_message_peer_list" begin fun () -> 
      let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
      P2p.broadcast simple_data_msg nodes.(1) >> 
      let%lwt check_message = message_check_thread nodes.(0)      
      in close_all nodes >> Lwt.return check_message
    end;

    test "simple_message_peer_file" begin fun () -> 
      let%lwt node_a = P2p.create ~port:4444 "nodes/node_a.peers" in
      let%lwt node_b = P2p.create ~port:4445 "nodes/node_b.peers" in
      P2p.broadcast simple_data_msg node_b >>
      let%lwt check_message = message_check_thread node_a
      in close_all [|node_a;node_b|] >> Lwt.return (check_message)
    end;

    test "simple_message_back_forth" begin fun () -> 
      let%lwt node_a = P2p.create ~peer_share:false ~port:4444 "nodes/node_a.peers" in
      let%lwt node_b = P2p.create ~peer_share:false ~port:4445 "nodes/node_b.peers" in
      P2p.broadcast simple_data_msg node_b >>
      let%lwt check_message_1 = message_check_thread node_a in       
      P2p.broadcast simple_data_msg node_a >>
      let%lwt check_message_2 = message_check_thread node_b  
      in close_all [|node_a;node_b|] >> Lwt.return (check_message_1 && check_message_2)
    end;

    test "simple_message_multicast" begin fun () -> 
      let%lwt node_a = P2p.create ~port:4444 "nodes/node_a.peers" in
      let%lwt node_b = P2p.create ~port:4445 "nodes/node_b.peers" in
      let%lwt node_c = P2p.create ~port:4446 "nodes/node_c.peers" in
      P2p.broadcast simple_data_msg node_b >>
      let%lwt check_message_1 = message_check_thread node_a in 
      let%lwt check_message_2 = message_check_thread node_c in
      close_all [|node_a; node_b; node_c|] >> Lwt.return (check_message_1 && check_message_2)
    end;

    test "peer_sync_test_explicit" begin fun () ->
      let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
      Lwt_unix.sleep 20. >> 
      let string_sort e1 e2 = 
        if e1 < e2 then (~-1) else if e1 > e2 then 1 else 0 in
      let known_peers_b = List.sort string_sort
          (List.map (fun peer -> (BRCPeer.s_addr peer)) (P2p.known_peers nodes.(1))) 
      in 
      close_all nodes >> Lwt.return 
        (known_peers_b = 
         ["127.0.0.1:3999";
          "127.0.0.1:4000"])
    end;

    test "peer_sync_test_share" begin fun () ->
      let%lwt node_a = P2p.create ~port:4443 "nodes/node_a.peers" in
      let%lwt node_b = P2p.create ~port:4445 "nodes/node_b.peers" in 
      Lwt_unix.sleep 20. >> 
      let string_sort e1 e2 = 
        if e1 < e2 then (~-1) else if e1 > e2 then 1 else 0 in
      let known_peers_a = List.sort string_sort
          (List.map (fun peer -> (BRCPeer.s_addr peer)) (P2p.known_peers node_a)) 
      in  
      let known_peers_b = List.sort string_sort
          (List.map (fun peer -> (BRCPeer.s_addr peer)) (P2p.known_peers node_b))  
      in  
      close_all [|node_a; node_b|] >> Lwt.return (known_peers_a = known_peers_b)
    end;

  ]

let tests = suites @ [messaging_tests]
let () = Lwt_test.run "all_tests" tests