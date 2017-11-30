

type command = (string*(string list))

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

module CommandParserImpl : CommandParser