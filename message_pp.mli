(** message.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_get_request : Format.formatter -> Message_types.get_request -> unit 
(** [pp_get_request v] formats v *)

val pp_get : Format.formatter -> Message_types.get -> unit 
(** [pp_get v] formats v *)

val pp_transaction_output : Format.formatter -> Message_types.transaction_output -> unit 
(** [pp_transaction_output v] formats v *)

val pp_transaction_input : Format.formatter -> Message_types.transaction_input -> unit 
(** [pp_transaction_input v] formats v *)

val pp_transaction : Format.formatter -> Message_types.transaction -> unit 
(** [pp_transaction v] formats v *)

val pp_block_header : Format.formatter -> Message_types.block_header -> unit 
(** [pp_block_header v] formats v *)

val pp_block : Format.formatter -> Message_types.block -> unit 
(** [pp_block v] formats v *)

val pp_peer : Format.formatter -> Message_types.peer -> unit 
(** [pp_peer v] formats v *)

val pp_manage_manage_t : Format.formatter -> Message_types.manage_manage_t -> unit 
(** [pp_manage_manage_t v] formats v *)

val pp_manage : Format.formatter -> Message_types.manage -> unit 
(** [pp_manage v] formats v *)

val pp_post : Format.formatter -> Message_types.post -> unit 
(** [pp_post v] formats v *)

val pp_message_method : Format.formatter -> Message_types.message_method -> unit 
(** [pp_message_method v] formats v *)

val pp_message : Format.formatter -> Message_types.message -> unit 
(** [pp_message v] formats v *)
