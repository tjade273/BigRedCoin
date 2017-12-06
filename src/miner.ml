(* A t is a miner with a push stream of blocks in push, and a list of mining
 * process ids with a read pipe and a write pipe. *)
type t = {
  push : Block.header option -> unit;
  pids : (int * Unix.file_descr * Unix.file_descr) list
}

let create f =
  {push = f; pids = []}

(* [mine r w p] attempts to mine blocks sourced from the read pipe [r] and writes
 * results to the write pipe [w]. The next block to be mined is in [p]. *)
let rec mine r w p =
  let b = Lwt_io.read r in
  if b = "stop" then
    close_in r;
    close_out w;
    ()
  else if b = "" then
    match p with
    | Some x -> 
        let next = {p with 
          timestamp = int_of_float (Unix.time ()); 
          nonce = b.nonce + 1 mod 2147483647 
        }
        if Block.hash p < Block.target p.nBits then
          Lwt_io.write w (Block.serialize p);
          mine r w None
        else
          mine r w next
    | None -> mine r w None
  else
    let x = Block.deserialize b in
    let y = {x with
      timestamp = int_of_float (Unix.time ());
      nonce = Random.int 217483647
    } in
    mine r w y

(* [manage t b] pulls a block from the blockchain and updates the miners in [t]
 * if the block is not [b]. When a miner finds a block, push it into the push
 * stream in [t]. *)
let manage t b =
  let%lwt newb = Blockchain.next_block () in
  if Some newb = b then
    let check (_, r, _) =
      let%lwt str = Lwt_io.read r
      if str = "" then 
        ()
      else 
        try t.push (Block.deserialize str) with
        | exn n -> ()
    List.iter check t.pids;
    manage t (Some b)
  else
    let f (_, _, wr) = write (Block.serialize b) w >>= ignore (flush w)
    List.iter f t.pids;
    manage t (Some newb)

let start t num =
  let rec start_instance t num =
    if num = 0 then 
      ()
    else
      let r = Lwt_io.pipe () in
      let w = Lwt_io.pipe () in
      let id = Unix.fork () in
      if id = 0 then mine (open_in (fst w)) (open_out (snd r)) None
    else
      start_instance 
        {t with pids = (t.pids :: (id, open_in (fst r), open_out (snd w)))} 
        (num - 1) in
  start_instance t num;
  manage t None

let stop t =
  let f = fun () x -> Unix.kill x Sys.sigterm in
  List.fold_left f t.mining_pids
