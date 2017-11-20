open Lwt
open Message_types

let rec print_message msg = 
  Lwt.return (print_endline "Got message")

let msg = {method_=(Message_types.Get);get=None;post=None}

let () = 
    let main = 
      let peer_a = P2p.create_from_list [("127.0.0.1",4445)] in
      let peer_b = P2p.create_from_list [("127.0.0.1",4444)] in 
      P2p.start_server ~port:4444 peer_a 
      >> P2p.start_server ~port:4445 peer_b 
      >> P2p.connect_to_known_peers peer_a 
      >> P2p.connect_to_known_peers peer_b       
      >> P2p.broadcast msg peer_b 
      >> Lwt_stream.iter_p print_message (P2p.in_message_stream peer_a) 
      >> Lwt_log.notice ("Stream ended") in
      Lwt_main.run (main)


