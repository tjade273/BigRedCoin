open Brc_repl
open ANSITerminal

let command_parser = CommandParserImpl.from_command_file "res/commands.json"

type t = 
  {
    mutable active_account: Accounts.t option 
  }

let main = {
      active_account=None
    }

let create_account_hook (command,args)  = 
  if command = "create" then 
    if Array.length args < 3 then 
      Some "Invalid number of arguments"
    else
      let dir = args.(0) in 
      let pass = args.(1) in 
      let new_account = Accounts.create dir pass in
      main.active_account <- Some new_account;
      Some ("Successfully created new account: "^dir)
  else
    None

let login_hook (command,args) = 
  if command = "login" then 
    let dir = args.(0) in 
    let pass = args.(1) in 
    match Accounts.load dir pass with
    | Some account ->
        main.active_account <- Some account;
        Some ("Successfully logged in to: " ^ dir)
    | None -> 
      Some"Invalid login"
  else
    None

(*[help_listener (command,args) repl hook that listens for the "help" command]*)
let help_hook (command,_) = 
  if command = "help" then       
    let fmt = format_of_string "| %s | \n\t %s \n" in
    let data = List.fold_left(fun acc (name,command) ->
      print_string [blue] name;
      (Printf.sprintf fmt name command.hint) ^ acc) "" (CommandParserImpl.commands command_parser)
    in 
      Some data
  else
    None

let () = 
  add_hook help_hook;
  add_hook login_hook;  
  add_hook create_account_hook;    
  run command_parser 