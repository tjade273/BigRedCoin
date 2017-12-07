open Lwt
open Block
open Transaction

(* The type of a miner for a specific blockchain, with a stream to push blocks
 * to and the address of the miner. *)
type t = {
  blockchain : Blockchain.t ref;
  push : Block.t option -> unit;
  mutable mining : bool;
  address : string
}

let create addr f chain =
  {push = f; blockchain = chain; mining = false; address = addr}

(* [equiv block1 block2] is true if [block1] equals [block2] in all fields but
 * the header's nonce and timestamp. 
 * requires: block1 and block2 both have a nonempty transaction. *)
let equiv block1 block2 =
  block1.header.version = block2.header.version
  && block1.header.merkle_root = block2.header.merkle_root
  && block1.header.prev_hash = block2.header.prev_hash
  && block1.header.nBits = block2.header.nBits
  && block1.transactions_count = block2.transactions_count
  && List.tl block1.transactions = List.tl block2.transactions

(* [mine t prev] attempts to mine blocks sourced from [t]'s blockchain and pushes
 * blocks with low enough hash to [t]'s push stream. The next block to be
 * mined is in [prev]. *)
let rec mine t prev =
  if t.mining = false then (Lwt.return ())
  else begin
    Lwt_main.yield () >>
    Blockchain.next_block !(t.blockchain) >>= fun b ->
      let b = {b with transactions = {
          ins = [];
          outs = [{amount = 25; address = t.address}];
          sigs = Some [Crypto.random 256]
        }::b.transactions;
        transactions_count = List.length b.transactions + 1
      } in
      match prev with
        | Some block -> begin
            if equiv b block then
              let next = Some Block.{block with
                Block.header = {block.header with
                  Block.timestamp = int_of_float (Unix.time ());
                  nonce = block.header.nonce + 1 mod 2147483647
                }
              } in
              if Block.hash block < Block.target block.header.nBits then begin 
                t.push (Some block);
                mine t None
              end
              else mine t next
            else
              let y = Some Block.{b with
                Block.header = {b.header with
                  Block.timestamp = int_of_float (Unix.time ());
                  nonce = Random.int 217483647
                }
              } in
              mine t y
            end
        | None -> mine t (Some b)
  end

let start t =
  t.mining <- true;
  Lwt.return (Lwt.async (fun _ -> mine t None))

let stop miner =
  miner.mining <- false
