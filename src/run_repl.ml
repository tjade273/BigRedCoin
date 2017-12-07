open Brc_repl
open ANSITerminal

let command_parser = CommandParserImpl.from_command_file "res/commands.json"

type t = 
  {
    mutable active_account: Accounts.t option;
    mutable p2p:P2p.t option
  }

let main = {
  active_account=None;
  p2p=None
}

let peers_hook (command,args) = 
  if command = "peers" then 
    match main.p2p with 
    | Some p2p -> 
      let peers_list = P2p.known_peers p2p in 
      let post = List.fold_left (fun acc e -> 
        acc ^ P2p.BRCPeer.s_addr e ^ "\n") "| Peers | \n" peers_list in 
        Lwt.return_some post
    | None -> Lwt.return_some "Peer not connected. Please see | launch |."
  else
    Lwt.return_none

let start_node_hook (command,args) = 
  if command = "launch" then 
    let port = int_of_string args.(0) in
    let%lwt server =  P2p.create ~port:port "peers/.peers" in 
    main.p2p <- Some server;
    Lwt.return_some "Launched server."
  else
    Lwt.return_none

let create_account_hook (command,args)  = 
  if command = "create" then 
    if Array.length args < 2 then 
      Lwt.return_some "Invalid number of arguments"
    else
      let dir = args.(0) in 
      let pass = args.(1) in 
      let new_account = Accounts.create dir pass in
      main.active_account <- Some new_account;
      Lwt.return_some ("Successfully created new account: "^dir^"/n"^
                       "Successfully logged in to: " ^ dir)
  else
    Lwt.return_none

let login_hook (command,args) = 
  if command = "login" then 
    let dir = args.(0) in 
    let pass = args.(1) in 
    match Accounts.load dir pass with
    | Some account ->
      main.active_account <- Some account;
      Lwt.return_some ("Successfully logged in to: " ^ dir)
    | None -> 
      Lwt.return_some "Invalid login"
  else
    Lwt.return_none

(*[help_listener (command,args) repl hook that listens for the "help" command]*)
let help_hook (command,_) = 
  if command = "help" then       
    let fmt = format_of_string "| %s | \n\t %s \n" in
    let data = List.fold_left(fun acc (name,command) ->
        print_string [blue] name;
        (Printf.sprintf fmt name command.hint) ^ acc) "" (CommandParserImpl.commands command_parser)
    in 
    Lwt.return_some data
  else
    Lwt.return_none



let () = 
  add_hook help_hook;
  add_hook login_hook;  
  add_hook create_account_hook;
  add_hook start_node_hook;
  add_hook peers_hook;
  Lwt_main.run (run command_parser)
