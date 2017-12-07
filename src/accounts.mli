exception InvalidPassword
exception InvalidAccountFormat
(* The type of an account, which tracks keys, addresses, and balances. *)
type t

(* [create dir pswd] creates a new account to be saved in the directory [dir]
 * encrypted with password [pswd]. *)
val create : string -> string -> t

(* [load dir pswd] reads the account at directory [dir] and attempts to decrypt
 * with password [pswd]. [None] on incorrect password. *)
val load : string -> string -> t option

(* [address a] gets a new address from account [a] to be used for receiving 
 * funds. *)
val address : t -> string

(* [addresses a] is all addresses for which [a] has the private key. *)
val addresses : t -> string list

(* [send_transaction a [(addr1, value1); ...; (addrn,valuen)] fee] sends 
 * [valuei] coins to [addri] and with the miner fee [fee]. [true] on 
 * success, [false] on insufficient balance. *)
val send_transaction : t -> string -> int -> int -> bool

(* [balance a addr_opt] is the total balance of all accounts owned by [a] if
 * [adr_opt] is [None], and the balance of [addr] when [addr_opt] is 
 * [Some addr]. *)
val balance : t -> Blockchain.t -> string option -> int


val dir : t -> string