(* A t is a miner with a pull stream of blocks in pull, a push stream of blocks
 * in push, and a list of mining process ids with a read pipe and a write pipe.
 * *)
type t = {
  pull : Block.header Lwt_stream.t;
  push : Block.header option -> unit;
  pids : (int * Unix.file_descr * Unix.file_descr) list
}

(* [mine r w p] attempts to mine blocks sourced from the read pipe [r] and writes
 * results to the write pipe [w]. The next block to be mined is in [p]. *)
let rec mine r w p =
  let b = input_line r in
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
          output_string w (Block.serialize p);
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

let start t num =
  let rec start_instance t num =
    if num = 0 then 
      ()
    else
      let r = Unix.pipe () in
      let w = Unix.pipe () in
      let id = Unix.fork () in
      if id = 0 then mine (open_in (fst w)) (open_out (snd r)) None
    else
      start_instance 
        {t with pids = (t.pids :: (id, open_in (fst r), open_out (snd w)))} 
        (num - 1) in
  start_instance t num

let stop t =
  let f = fun () x -> Unix.kill x Sys.sigterm in
  List.fold_left f t.mining_pids
