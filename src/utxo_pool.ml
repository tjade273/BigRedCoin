open Transaction
open Block
exception Invalid_block

let output_hash tx_id output_index = 
  Crypto.sha256 (tx_id^(string_of_int output_index))

module StringMap = Map.Make(String)

type t = {pool :(Transaction.output) StringMap.t}

let empty = {pool = StringMap.empty}


let revert_transaction tm transaction = 
  let trans_hash = Transaction.hash transaction in 
  let removed = List.fold_left (fun (acc,index) output -> 
    let hash = output_hash trans_hash index in     
    (StringMap.remove hash acc,index+1)
  ) (tm,0) transaction.outs
in
  let readded_inputs = 
    List.fold_left (fun acc input -> 
        let hash = output_hash input.txid input.out_index in
        let old_transaction = TransactionDB.get input.txid in 
        let old_output = List.nth old_transaction.outputs input.out_index in
        (StringMap.add hash old_output acc)
      ) (fst removed) transaction.ins
  in
  readded_inputs

let put_transaction tm transaction = 
  let removed = 
    List.fold_left (fun acc input -> 
        let hash = output_hash input.txid input.out_index in 
        (StringMap.remove hash acc)
      ) tm transaction.ins
  in
  let trans_hash = Transaction.hash transaction in
  let added = List.fold_left (fun (acc,index) output -> 
      let hash = output_hash trans_hash index in 
      (StringMap.add hash output acc,index+1)
    ) (removed,0) transaction.outs
  in
  fst added

(*verify inputs exist*)
let verify transaction = 
  match (Transaction.signers transaction) with 
  | None ->  false  
  | Some signers ->
    List.for_all(fun output -> (List.mem output.address signers)) transaction.outs

let revert p (b:Block.t) = 
  let transactions = b.transactions in 
  List.fold_left (fun acc t -> revert_transaction p t) p transactions
  
let apply p block = 
  if (List.for_all verify block.transactions) then 
     {pool = List.fold_left put_transaction p.pool block.transactions}
  else
    raise Invalid_block