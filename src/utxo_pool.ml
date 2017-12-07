open Transaction
open Block
module TransactionDB = Database.Make(Transaction)

exception Invalid_block

let output_hash tx_id output_index =
  Crypto.sha256 (tx_id^(string_of_int output_index))

module UTXO = struct
  type t = Transaction.input
  let compare = compare
end

module UTXOMap = Map.Make(UTXO)

type t = {pool : Transaction.output UTXOMap.t; db : TransactionDB.t}

let empty db = {pool = UTXOMap.empty; db}


let revert_transaction {pool; db} transaction =
  let trans_hash = Transaction.hash transaction in
  let removed = List.fold_left (fun (acc,index) output ->
      let utxo = {txid = trans_hash; out_index = index} in
      (UTXOMap.remove utxo acc,index+1)
    ) (pool,0) transaction.outs
  in
  let%lwt readded_inputs =
    let add_output acc input =
      let%lwt old_transaction = TransactionDB.get db input.txid in
      let old_output = List.nth old_transaction.outs input.out_index in
      Lwt.return @@ UTXOMap.add input old_output acc
    in
    Lwt_list.fold_left_s add_output (fst removed) transaction.ins
  in
  Lwt.return {pool = readded_inputs; db}

(*verify inputs exist*)
let verify pool ({ins; outs; sigs} as transaction) =
  try
    match (Transaction.signers transaction) with
    | None -> raise Invalid_block
    | Some signers ->
      begin
        let is_signed =
          List.for_all
            (fun input -> (List.mem (UTXOMap.find input pool).address signers))
            ins
        in
        let zero_sum =
          let out_sum =
            List.fold_left (fun acc {amount; _} -> acc + amount) 0 outs
          in
          let in_amnt input = (UTXOMap.find input pool).amount in
          let in_sum = List.fold_left (+) 0 (List.map in_amnt ins) in
          in_sum >= out_sum
        in
        let inputs_unique = List.sort compare ins = List.sort_uniq compare ins in
        if not (is_signed  && zero_sum && inputs_unique)
        then raise Invalid_block
      end
  with _ -> raise Invalid_block

let put_transaction tm transaction =
  verify tm transaction;
  let removed =
    List.fold_left (fun acc input ->
        (UTXOMap.remove input acc)
      ) tm transaction.ins
  in
  let txid = Transaction.hash transaction in
  let added = List.fold_left (fun (acc,out_index) output ->
      (UTXOMap.add {txid; out_index} output acc, out_index+1)
    ) (removed,0) transaction.outs
  in
  fst added

(* The first (index 0) transaction in a block may be a "coinbase" transaction.
 * This transaction has 0 inputs an a single 25-brc output. To differentiate
 * coinbase transactions, the sigs field should contain arbitrary random data. *)
let add_coinbase pool ({ins; outs; sigs} as tx) =
  match ins, outs with
  | [], ({amount = 25; _} as cbs)::[] ->
    UTXOMap.add
      ({txid = Transaction.hash tx; out_index = 0}) cbs pool
  | _ -> put_transaction pool tx

let revert p (b:Block.t) =
  let transactions = b.transactions in
  let%lwt pool = Lwt_list.fold_left_s revert_transaction p transactions in
  Lwt.return pool

let apply {pool; db} block =
  Lwt_list.iter_p (TransactionDB.put db) block.transactions >>
  match block.transactions with
  | [] -> Lwt.return {pool; db}
  | coinbase::txs ->
    let pool' = add_coinbase pool coinbase in
    Lwt.return
      {db; pool = List.fold_left put_transaction pool' txs}


let filter {pool; _} addr =
  UTXOMap.filter (fun input {address; _} -> address = addr) pool
  |> UTXOMap.bindings
