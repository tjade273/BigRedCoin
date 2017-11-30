
type command = (string*(string list))

module type CommandParser = sig 
  type t
  val parse  : t -> string -> command option

  val from_command_file : string -> t
end

module CommandParserImpl : CommandParser