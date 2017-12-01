

type command = (string*(string list))
type command_hook = command -> string option


type command_dir = 
  {
    hint:string;
    name:string;
    regexes: Str.regexp list;
  }

(*module that offerse command parsing*)
module type CommandParser = sig 
  type t
  val parse : ?parse_error_callback:(string -> unit) -> t -> string -> command option
  val from_command_file : string -> t
  val commands : t -> (string*command_dir) list 
end

module CommandParserImpl : CommandParser

(*[add_hook hook] adds a command_hook to the repls hooks list. The hook 
 * will be notified when the repl recieves a valid command. *)
val add_hook : command_hook -> unit

(*[run] runs the repl continuously.*)
val run : CommandParserImpl.t -> unit