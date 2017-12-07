(* The type for a command to the repl, includes an action and the list of
 * arguments provided *)
type command = (string * (string array))

(* The type for repl hooks that are listeners for specific commands *)
type command_hook = command -> string option Lwt.t

(* The type for command parsing instructions *)
type command_dir =
  {
    hint: string;
    name: string;
    regexes: Str.regexp list;
  }

(* [CommandParser] is a module that is used to parse commands.*)
module type CommandParser = sig
  (* The type for the command parser *)
  type t

  (* [parse t input] is a [command] which is the given [input] parsed using
   * the parser [t]. If the input is not parsed correctly, [None] is returned.
   * There is an option [parse_error_callback] which can be provided to be run
   * in the case the [input] is not parsed. *)
  val parse : ?parse_error_callback:(string -> unit) -> t -> string ->
    command option

  (* [from_command_file file_path] is a parser created using the list of commands
   * in the file specified by path [file_path]. *)
  val from_command_file : string -> t

  (* [commands parser] is a list of commands and the associated parsing rules
   * used by the given [parser] *)
  val commands : t -> (string * command_dir) list
end

(* [CommandParserImpl] is a [CommandParser]*)
module CommandParserImpl : CommandParser

(* [add_hook hook] adds a command_hook to the repl's hooks list. The hook
 * will be notified when the repl recieves a valid command. *)
val add_hook : command_hook -> unit

(* [run parser] runs the repl perpetually, repeatedly prompting the user for inputs,
 * parsing the input using [parser] and triggering the specific hook. *)
val run : CommandParserImpl.t -> unit Lwt.t
