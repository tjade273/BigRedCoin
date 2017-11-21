open Lwt
open Message_types
open P2p

let suites : Test.suite list = []

open Test

let close_all (node_lst:P2p.t array) = 
  Array.fold_left (fun acc node -> 
    (P2p.shutdown node)<&>acc) Lwt.return_unit node_lst

let rec create_n_linked_nodes ?start_port:(start_port=4000) n =
  let rec create_nodes_rec p lst =
    if p > start_port + n then  
      Lwt.return (Array.of_list lst)
    else
      let%lwt new_node =  P2p.create_from_list ~port:p [("127.0.0.1",p-1)] in
      create_nodes_rec (p+1) (lst @ [new_node])
    in
    create_nodes_rec start_port []

let messaging_tests = suite "messaging tests" [

    test "test_test" begin fun () ->
      Lwt.return true
    end;

    test "open_server" begin fun () ->
      try let%lwt p2p = P2p.create_from_list ~port:4444 [("127.0.0.1",4445)]
        in  P2p.shutdown p2p >> Lwt.return true
      with 
      | _ -> Lwt.return false
    end;

    test "simple_message" begin fun () -> 
      let msg = {method_=(Message_types.Get);get=None;post=None} in      
      let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
      P2p.broadcast msg nodes.(1) >>
      let res = Lwt_stream.get (P2p.peer_stream nodes.(0)) in
        let%lwt check_message = match%lwt res with 
        | Some peer ->  (nodes.(0),peer) @<> (fun peer ->
            (match%lwt BRCMessage_channel.read (BRCPeer.ic peer) with
            | Some msg -> Lwt.return (true,Lwt.return true)
            | None -> Lwt.return (true,Lwt.return false))) 
        |None -> Lwt.return false
      in close_all nodes >> Lwt.return check_message
    end;

  test "test_connect_random" begin fun () -> 
    let%lwt nodes = create_n_linked_nodes ~start_port:4000 2 in
    let peer_opt = Lwt_stream.get (P2p.peer_stream nodes.(1)) in
    let%lwt check_connection = 
      match%lwt peer_opt with 
        | Some peer -> Lwt.return true 
        | None -> Lwt.return false;
    in close_all nodes >> Lwt.return check_connection
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