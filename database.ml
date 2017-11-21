module type Hashable = sig
  type t

  val hash : t -> string

  val serialize : t -> string

  val deserialize : string -> t
end

module type S = sig

  type t

  type value

  val put : t -> value -> unit Lwt.t

  val get : t -> string -> value

  val get_opt : t -> string -> value option
      
  val create : string -> t

  val mem : t -> string -> bool Lwt.t

  val remove : t -> string -> unit Lwt.t

end

module Make(Value : Hashable) : S = struct

  type t = LevelDB.db

  type value = Value.t

  let create dir = LevelDB.open_db dir

  let put db v = 
    let f (db, v) = LevelDB.put db (Value.hash v) (Value.serialize v) in
    Lwt_preemptive.detach f (db, v)

  let get db v = 
    let f (db, v) = Value.deserialize (LevelDB.get_exn db (Value.hash v)) in
    Lwt_preemptive.detach f (db, v)

  let get_opt db v = 
    let f (db, v) =
      match LevelDB.get db (Value.hash v) with 
      | None -> None
      | Some s -> Value.deserialize s in
    Lwt_preemptive.detach f (db, v)

  let mem db v = 
    let f (db, v) = LevelDB.mem db (Value.hash v) in
    Lwt_preemptive.detach f (db, v)

  let remove db v =
    let f (db, v) = LevelDB.delete db (Value.hash v) in
    Lwt_preemptive.detach f (db, v)

end
