open ANSITerminal
open Yojson.Basic.Util

type command = (string * (string array))
type command_hook = command -> string option Lwt.t

type command_dir =
  {
    hint: string;
    name: string;
    regexes: Str.regexp list;
  }

module type CommandParser = sig
  type t
  val parse : ?parse_error_callback:(string -> unit) -> t -> string ->
    command option
  val from_command_file : string -> t
  val commands : t -> (string * command_dir) list
end

module CommandParserImpl = struct
  type t =
    { commands: (string * command_dir) list }

  let parse ?parse_error_callback:(cb = fun error -> ()) t input =
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
        (Some (command_head,Array.of_list (Str.split (Str.regexp_string " ")
                                             command_tail)))
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
    in { commands = commands}

  let commands parser =
    parser.commands
end

(* The type for a repl. *)
type t = {
  mutable hooks:(command -> string option Lwt.t) list;
  mutable error_lst:string list;
  mutable data_lst:string list;
  mutable running:bool;
}

(* An instance of a repl. *)
let repl = {
  hooks = [];
  error_lst = [];
  data_lst = [];
  running = true;
}

(* [header] is the text-based graphic to be displayed at the beginning of the
 * repl. It includes the logo. *)
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

(* [print_header ()] prints the [header] to output console for the [repl]. *)
let print_header () =
  print_newline ();
  List.iteri(fun n line -> set_cursor 45 (n+1);  print_string [red] line;) header;
  print_newline ()

let add_hook listener  =
  repl.hooks <- listener::repl.hooks

(* [store_error error] adds the given [error] to the list of errors in the
 * [repl]. *)
let store_error (error:string) =
  repl.error_lst <- error::repl.error_lst

(* [store_data data] adds the given [data] to the data list in the [repl]. *)
let store_data (data:string) =
  repl.data_lst <- data::repl.data_lst

(* [post_errors ()] prints the errors in the [repl] to the console. *)
let post_errors () =
  List.iter (fun error ->
      ANSITerminal.set_cursor 5 (73 - (List.length repl.error_lst));
      print_string [red] error) repl.error_lst;
  repl.error_lst <- []

(* [count_char str ch] counts the number of occurences of [ch] in [str]. *)
let count_char str ch =
  let count = ref 0 in
  String.iter (fun c -> if c = ch then count := !count+1) str;
  !count

(* [post_data ()] prints the data in the [repl] to the console. *)
let post_data () =
  let new_lines = List.fold_left
      (fun acc data -> acc + (count_char data '\n')) 0 repl.data_lst in
  List.iter (fun data ->
      ANSITerminal.set_cursor 1 (58-new_lines);
      print_string [white] data) repl.data_lst

(* [handle_input parser] reads input from stdin and attemps to parse it
 * into a command using the given [parser]. *)
let handle_input parser =
  ANSITerminal.set_cursor 10 60;
  print_string [red] ">";
  let%lwt input = (Lwt_io.read_line Lwt_io.stdin) in
  let command =
    CommandParserImpl.parse parser input ~parse_error_callback:(store_error) in
  match command with
  | Some command ->
    repl.data_lst <- [];
    let threads = List.fold_left (fun acc (f:command -> string option Lwt.t) ->
          (match%lwt (f command) with
            | Some data -> store_data data; Lwt.return_unit
            | None -> Lwt.return_unit)::acc) [] repl.hooks
          in
            Lwt.join threads >>  
              (if (fst command) = "quit" then
                repl.running <- false;
              Lwt.return_unit)
  | None -> Lwt.return_unit

let rec run parser =
  ANSITerminal.resize 200 75;
  ANSITerminal.erase ANSITerminal.Screen;
  print_header ();
  post_errors();
  post_data ();
  handle_input parser >>
  if repl.running then 
    run parser
  else
    Lwt.return_unit
