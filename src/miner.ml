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

let write w s =
  let len_buffer = Cstruct.create 8 in
  let len = String.length s in
  Cstruct.BE.set_uint64 len_buffer 0 (Int64.of_int len);
  let l = Cstruct.to_string len_buffer in
  Lwt_io.write w (l^s)

let read r =
  let%lwt len_str = Lwt_io.read ~count:8 r in
  let len = Cstruct.BE.get_uint64 (Cstruct.of_string len_str) 0 in
  Lwt_io.read ~count:(Int64.to_int len) r

(* [equiv block1 block2] is true if [block1] equals [block2] in all fields but
 * the header's nonce and timestamp. *)
let equiv block1 block2 =
  block1.header.version = block2.header.version
  && block1.header.merkle_root = block2.header.merkle_root
  && block1.header.prev_hash = block2.header.prev_hash
  && block1.header.nBits = block2.header.nBits
  && block1.transactions = block2.transactions
  && block1.transactions_count = block2.transactions_count

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
          sigs = Some []
        }::b.transactions
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
              if Block.hash block < Block.target block.header.nBits then
                Lwt.return (t.push (Some block))
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

let start miner =
  miner.mining <- true;
  mine miner None

let stop miner =
  miner.mining <- false
