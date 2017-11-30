open Brc_repl
open ANSITerminal

let command_parser = CommandParserImpl.from_command_file "res/commands.json"

(*[help_listener (command,args) repl hook that listens for the "help" command]*)
let help_listener (command,args) = 
  if command = "help" then       
    let fmt = format_of_string "| %s | \n\t %s \n" in
    let data  =List.fold_left(fun acc (name,command) ->
      print_string [blue] name;
      (Printf.sprintf fmt name command.hint) ^ acc) "" (CommandParserImpl.commands command_parser)
    in 
      Some data
  else
    None

  

let () = 
  add_hook help_listener;
  run command_parser ()