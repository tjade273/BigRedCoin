open Lwt

module Chain = struct
  module M = Map.Make(String)
  type t = {head : string; parent : string; recent_blocks : int M.t; genesis : string}

  let shared_root c1 c2 =
    try
      List.find
          (fun (k,_) -> M.find_opt k c2.recent_blocks <> None)
          (M.bindings c1.recent_blocks) |> fst
    with Not_found -> c2.genesis

  let reorg c1 c2 =
    let root = shared_root c1 c2 in
    let remove c =

end

module TxDB = Database.Make(Transaction)
module BlockDB = Database.Make(Block)

(* Maps txids to transactions waiting to be included in a block *)
type mempool = (string, Transaction.t) Hashtbl.t

(* Maps block hash to utxo *)
type utxopool = (string, Transaction.output) Hashtbl.t

type t = {mempool : mempool; blockdb: BlockDB.t; utxopool : utxopool; head : Block.header}

let create dir =
  let parse_genesis ic =
    Lwt_io.read ic >>= fun s -> Lwt.return (Block.deserialize s)
  in
  let absolute = Filename.concat dir in
  let mempool = Mempool.create (absolute "mempool") in
  let blockdb = BlockDB.create (absolute "blocks") in
  let with_genesis = Lwt_io.(with_file ~mode:input (absolute "genesis.blk")) in
  let%lwt genesis = with_genesis parse_genesis in
  {mempool; blockdb; head = genesis}

