open Crypto
open Yojson

exception InvalidPassword
exception InvalidAccountFormat

let accounts_file_path = "/.brc_keys"

(* The type of an account, which tracks keys, addresses, and balances. *)
type t = 
  {
    account_path:string;
    mutable key_pairs:(ECDSA.pubkey*ECDSA.privkey)list;
    pswd:string
  } 

let save (a:t) = 
  let json_arr = List.map (fun key_pair ->
    let encrypted = AES.encrypt key_pair a.pswd in 
    let json_obj_str = AES.to_string encrypted in 
    Yojson.Basic.from_string json_obj_str
  ) a.key_pairs in 
  Basic.to_file (a.account_path^accounts_file_path) (`List json_arr)

(* [create dir pswd] creates a new account to be saved in the directory [dir]
 * encrypted with password [pswd]. *)
let create dir pswd = 
  let key_pair = ECDSA.create () in 
  let account = {
    account_path = dir^accounts_file_path;
    key_pairs = [key_pair];
    pswd = pswd    
  } in 
  save account;
  account

(* [load dir pswd] reads the account at directory [dir] and attempts to decrypt
 * with password [pswd]. [None] on incorrect password. *)
let load dir pswd = 
  let json_accounts = Basic.from_file (dir^accounts_file_path) in
  let key_pairs = Basic.Util.convert_each (fun json_key_pair -> 
    match AES.of_string (Yojson.Basic.to_string json_key_pair) with 
    | Some encr_key_pair -> 
      (match AES.decrypt encr_key_pair pswd with 
      | Some key_pair -> key_pair  
      | None -> raise InvalidPassword)
    | None -> raise InvalidAccountFormat
    ) json_accounts 
  in 
    { 
      account_path = dir;
      key_pairs = key_pairs;
      pswd = pswd
    }
 
let add_key_pair a kp = 
  a.key_pairs <- kp::a.key_pairs;
  save a
(* [address a] gets a new address from account [a] to be used for receiving 
 * funds. *)
let address a = 
  let key_pair = ECDSA.create () in
  add_key_pair a key_pair;
  ECDSA.to_address (fst key_pair)

(* [addresses a] is all addresses for which [a] has the private key. *)
let addresses a = 
  List.map(fun (pub,_) -> (ECDSA.to_address pub)) a.key_pairs

(* [balance a addr_opt] is the total balance of all accounts owned by [a] if
 * [adr_opt] is [None], and the balance of [addr] when [addr_opt] is 
 * [Some addr]. *)
 let balance a bc addr_opt : int = 
    match addr_opt with 
    | None -> List.fold_left (fun acc (pub,_) -> 
      let utxos  = (Blockchain.get_utxos bc (ECDSA.to_address pub)) in
      let tot_for_addr = (List.fold_left (fun tot utxo -> utxo.amount + tot) 0) utxos in 
       tot_for_addr + acc) 0 a.key_pairs
    | Some addr -> Blockchain.get_utxos addr 

(* [send_transaction a [(addr1, value1); ...; (addrn,valuen)] fee] sends 
 * [valuei] coins to [addri] and with the miner fee [fee]. [true] on 
 * success, [false] on insufficient balance. *)
let send_transaction a sub_transactions fee bc = 
  let total = List.fold_left (+) 0 sub_transactions in
  if total < (balance a bc None) then 
    false
  else
    Blockchain. sub_transactions bc;
    true

