open ANSITerminal
open Yojson.Basic.Util


type command = (string*(string list))
type command_hook = command -> string option


type command_dir = 
  {
    hint:string;
    name:string;
    regexes: Str.regexp list;
  }

module type CommandParser = sig 
  type t
  val parse : ?parse_error_callback:(string -> unit) -> t -> string -> command option

  val from_command_file : string -> t
end

module CommandParserImpl = struct 
  type t = 
    {commands:(string*command_dir) list}

  let parse ?parse_error_callback:(cb = fun error -> ()) t input   = 
    let input = (String.trim input) in
    let first_space = 
      match String.index_opt input ' ' with
      | Some i -> i
      | None -> (String.length input)
    in 
    let command_head = Str.string_before input first_space in 
    let command_tail = if first_space = (String.length input) then "" 
      else Str.string_after input (first_space + 1)
    in
    match List.assoc_opt command_head t.commands with 
    | Some command -> 
      if List.exists 
          (fun regex -> Str.string_match regex input 0) command.regexes 
      then
        (Some (command_head,Str.split (Str.regexp_string " ") command_tail))    
      else
        let valid_regex_msg = 
            ("Invalid format for " ^ command_head ^ ": \n\t" ^ command.hint) in 
        cb valid_regex_msg;
        (None)
    | None -> cb ("Unknown command: "^command_head); None

  let from_command_file file_path = 
    let json_struct = Yojson.Basic.from_file file_path in 
    let json_commands = json_struct |> member "commands" in 
    let commands = convert_each (fun json_command -> 
        let name = json_command |> member "name" |> to_string in
        let hint = json_command |> member "hint" |> to_string in
        let json_regexes = json_command |> member "regex" in 
        let regexes = convert_each (fun json_regex -> 
          let regex_str = (to_string json_regex) in 
          (Str.regexp regex_str)) json_regexes in 
      (name,{name=name;hint=hint;regexes=regexes})) json_commands
               
    in {commands = commands}

end

let command_parser = CommandParserImpl.from_command_file "res/commands.json"

type t = {
  mutable hooks:(command -> string option) list;
  mutable error_lst:string list;
  mutable data_lst:string list;  
}

let repl = {
  hooks = [];
  error_lst = [];
  data_lst = []
}

(*[help_listener (command,args) repl hook that listens for the "help" command]*)
let help_listener (command,args) = 
  if command = "help" then       
    let fmt = format_of_string "| %s | \n\t %s \n" in
    let data  =List.fold_left(fun acc (name,command) ->
      print_string [blue] name;
      (Printf.sprintf fmt name command.hint) ^ acc) "" command_parser.commands
    in 
      Some data
  else
    None

  

let header = 
  let ic = open_in "res/logo.txt" in
  let rec read_all (ic:in_channel) lst = 
    try 
      let line = input_line ic  in  
      let s = lst @ [line ^ "\n"] in 
      read_all ic s 
    with
    | End_of_file -> 
      close_in ic; lst
  in
  read_all ic []                 

let print_header () = 
  print_newline ();
  List.iteri(fun n line -> set_cursor 45 (n+1);  print_string [red] line;) header; 
  print_newline ()

let add_hook listener  = 
  repl.hooks <- listener::repl.hooks

let store_error (error:string) = 
  repl.error_lst <- error::repl.error_lst 

let store_data (data:string) = 
  repl.data_lst <- data::repl.data_lst

let post_errors () = 
  List.iter (fun error -> 
    ANSITerminal.set_cursor 5 (73 - (List.length repl.error_lst)); 
    print_string [red] error) repl.error_lst;
  repl.error_lst <- []

(*[count_char str ch] counts the number of occurances of [ch] in [str]*)
let count_char str ch = 
  let count = ref 0 in
  String.iter (fun c -> if c = ch then count := !count+1) str; 
  !count

let post_data () = 
  let new_lines = List.fold_left(fun acc data -> acc + (count_char data '\n')) 0 repl.data_lst in
  List.iter (fun data -> 
  ANSITerminal.set_cursor 1 (58-new_lines); 
  repl.data_lst <- [];
  print_string [white] data) repl.data_lst

(*[handle_input ()] reads input from stdin and attemps to parse into a command.*)
let handle_input () = 
  ANSITerminal.set_cursor 10 60;    
  print_string [red] "> ";
  let input = read_line () in 
  let command = CommandParserImpl.parse command_parser input ~parse_error_callback:(store_error) in
  match command with 
  | Some command -> 
    List.iter (fun (f:command -> string option) ->
       match (f command) with
       | Some data -> store_data data
       | None -> ()) repl.hooks
  | None -> ()

(*[run ()] runs repl continuously*)
let rec run () = 
  ANSITerminal.erase ANSITerminal.Screen;
  ANSITerminal.resize 200 75;
  print_header ();
  post_errors();
  post_data ();
  handle_input ();
  run ()

let () = 
  add_hook help_listener;
  run ()