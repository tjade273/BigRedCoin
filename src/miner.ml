open Lwt
open Block
(* A t is a miner with a push stream of blocks in push, and a list of mining
 * process ids with a read pipe and a write pipe. *)
type t = {
  blockchain : Blockchain.t ref;
  push : Block.t option -> unit;
  pids : (int * Lwt_io.input_channel * Lwt_io.output_channel) list
}

let create f chain =
  {push = f; pids = []; blockchain = chain}

(* [write w s] writes the length of the string [s] followed by [s] to [w]. *)
let write w s =
  let len_buffer = Cstruct.create 8 in
  let len = String.length s in
  Cstruct.BE.set_uint64 len_buffer 0 (Int64.of_int len);
  let l = Cstruct.to_string len_buffer in
  Lwt_io.write w (l^s)

(* [read r] is the next message in [r]. *)
let read r =
  let%lwt len_str = Lwt_io.read ~count:8 r in
  let len = Cstruct.BE.get_uint64 (Cstruct.of_string len_str) 0 in
  Lwt_io.read ~count:(Int64.to_int len) r

(* [mine r w p] attempts to mine blocks sourced from the read pipe [r] and writes
 * results to the write pipe [w]. The next block to be mined is in [p]. *)
let rec mine r w p =
  read r >>= fun b ->
    if b = "stop" then begin
      let _ = Lwt_io.close r in
      Lwt_io.close w
    end
    else begin
      if b = "" then
        match p with
        | Some block -> begin 
            let next = Some Block.{block with
              Block.header = {block.header with
                Block.timestamp = int_of_float (Unix.time ()); 
                nonce = block.header.nonce + 1 mod 2147483647
              }
            } in
            if Block.hash block < Block.target block.header.nBits then begin
                let _ = write w (Block.serialize block) in
                mine r w None
              end
            else
              mine r w next
          end
        | None -> mine r w None
      else
        let block = Block.deserialize b in
        let y = Some Block.{block with
          Block.header = {block.header with
            Block.timestamp = int_of_float (Unix.time ());
            nonce = Random.int 217483647
          }
        } in
        mine r w y
    end

(* [manage t b] pulls a block from the blockchain and updates the miners in [t]
 * if the block is not [b]. When a miner finds a block, push it into the push
 * stream in [t]. *)
let rec manage t b =
  let%lwt newb = Blockchain.next_block !(t.blockchain) in
  if (Some newb) = b then begin
      let check (_, r, _) =
        let f r = 
          let%lwt str = read r in
          if str = "" then 
            Lwt.return ()
          else
          try Lwt.return (t.push (Some (Block.deserialize str))) with
          | _ -> Lwt.return () in
        ignore (f r) in
      List.iter check t.pids;
      manage t b
    end
  else begin
    let f (_, _, wr) = 
      write wr (Block.serialize newb) >>= fun _ -> (Lwt_io.flush wr) in
    List.iter (fun x -> ignore (f x)) t.pids;
    manage t (Some newb)
  end

let start t num =
  let rec start_instance t num =
    if num = 0 then 
      ()
    else
      let r = Lwt_io.pipe () in
      let w = Lwt_io.pipe () in
      let id = Unix.fork () in
      if id = 0 then 
        ignore(mine (fst w) (snd r) None)
      else
        start_instance 
          {t with pids = (id, (fst r), (snd w))::t.pids} 
          (num - 1) in
  start_instance t num;
  ignore (manage t None)

let stop t =
  let f = fun () (x, _, _) -> Unix.kill x Sys.sigterm in
  List.fold_left f () t.pids
