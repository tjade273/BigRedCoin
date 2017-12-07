open Brc_repl
open ANSITerminal

(* [command_parser] is parser for the repl. *)
let command_parser = CommandParserImpl.from_command_file "res/commands.json"

(* The type used to hold state for the repl. *)
type t =
  {
    mutable active_account: Accounts.t option;
    mutable p2p:P2p.t option;
    mutable bc:Blockchain.t option
  }

(* [main] is an instance of [t] used to maintain state for the repl. *)
let main = {
  active_account=None;
  p2p=None;
  bc=None
}

let lookup_hook (command,args) =
  if command = "lookup" then
    let hash = Hex.to_string (`Hex args.(0)) in
    match main.bc with 
    | Some bc ->  
      (match%lwt Blockchain.retrieve_block bc hash with 
       | Some block -> Lwt.return_some "Valid block hash."
       | None -> Lwt.return_some "Invalid block hash.")
    | None -> Lwt.return_some "No chain found."
  else
    Lwt.return_none

let chain_head_hook (command,args) =
  if command = "chain_head" then
    match main.bc with 
    | Some bc -> Lwt.return_some ("| Chain Head | \n" ^ 
      Hex.show (Hex.of_string (Blockchain.head bc)))
    | None -> Lwt.return_some "No chain found."
  else
    Lwt.return_none

(* [peers_hook (commands,args)] is a repl hook that listens for the "peers"
 * command and prints a list of the known peers of running P2P node. *)
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

(* [start_node_hook (command,args)] is a repl hook that listens for the
 * "launch" command and starts a P2P node server on the port provided via [args]
 * requires: length of args >= 1. *)
let start_node_hook (command,args) =
  if command = "launch" then
    let port = int_of_string args.(0) in
    let%lwt server =  P2p.create ~port:port "peers/.peers" in
    main.p2p <- Some server;
    let%lwt bc = Blockchain.create "brc_chain" server in 
    main.bc <- Some bc;
    Lwt.return_some "Launched server."
  else
    Lwt.return_none

(* [create_account_hook (command,args)] is a repl hook that listens for the
 * "create" command and opens an account with details specified by the arguments
 * provided. It also logs into the newly created account.
 * requires: length of [args] >= 2. *)
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

(* [login_hook (command,args)] is a repl hook that listens for the "login"
 * command and logs into the existing account specified by the arguments provided.
 * requires: length of [args] >= 2. *)
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

(* [help_hook (command,args)] is a repl hook that listens for the "help"
 * command and prints the list of valid commands for the repl. *)
let help_hook (command,_) =
  if command = "help" then
    let fmt = format_of_string "| %s | \n\t %s \n" in
    let data = List.fold_left(fun acc (name,command) ->
        print_string [blue] name;
        (Printf.sprintf fmt name command.hint) ^ acc) ""
        (CommandParserImpl.commands command_parser)
    in
    Lwt.return_some data
  else
    Lwt.return_none

(* Add hooks to the repl, and run it.*)
let () =
  add_hook lookup_hook;  
  add_hook chain_head_hook;  
  add_hook help_hook;
  add_hook login_hook;
  add_hook create_account_hook;
  add_hook start_node_hook;
  add_hook peers_hook;
  Lwt_main.run (run command_parser)
