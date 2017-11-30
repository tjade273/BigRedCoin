open ANSITerminal
open Yojson.Basic.Util

type command = (string*(string list))

module type CommandParser = sig 
  type t
  val parse  : t -> string -> command option

  val from_command_file : string -> t
end

module CommandParserImpl = struct 
  type t = 
    {commands:(string*(Str.regexp list)) list}

  let parse t input = 
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
    | Some regexes -> 
        if List.exists 
          (fun regex -> Str.string_match regex input 0) regexes 
        then
          Some (command_head,Str.split (Str.regexp_string " ") command_tail)    
        else
          None
    | None -> None

  let from_command_file file_path = 
    let json_struct = Yojson.Basic.from_file file_path in 
    let json_commands = json_struct |> member "commands" in 
    let commands = convert_each (fun json_command -> 
      let name = json_command |> member "name" |> to_string in
      let json_regexes = json_command |> member "regex" in 
      let regexes = convert_each 
        (fun json_regex -> Str.regexp (to_string json_regex)) json_regexes in 
        (name,regexes)) json_commands
     in {commands = commands}

end

let command_parser = CommandParserImpl.from_command_file "res/commands.json"

type t = {
  listeners:(command -> string) list
}

let header = 
  let ic = open_in "res/logo.txt" in
  let rec read_all (ic:in_channel) s = 
    try 
      let line = input_line ic  in  
      let s = s ^ line ^ "\n" in 
      read_all ic s 
    with
    | End_of_file -> 
    close_in ic; s
  in
    read_all ic ""                 

let add_listener listener repl = 
    {repl with listeners = listener::(repl.listeners)}

let handle_input () = 
  let input = read_line() in 
  let command = CommandParserImpl.parse command_parser input in
  command

let print_header () = 
  print_newline ();
  print_string [red] header;
  print_newline ()

let run () = 
  print_header ()

(*let () = run ()module type BRCRepl_listener = sig 
  type t 
  val on_user_input : command -> string
end*)