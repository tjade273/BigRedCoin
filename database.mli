module type Hashable

module type S = sig
  
  (* [t] is the type representaing a database. *)
  type t

  (* A hashable value for use in the database. *)
  type value

  (* [put t b] takes in a database [t] and a value [b], and writes the block to
   * the database. *)
  val put : value t -> value -> unit Lwt.t

  (* [get t s] takes in a database [t] and the SHA-256 hash [s] of an value, and 
   * returns the value with that hash. Throws an exception if the value isn't 
   * present. *)
  val get : value t -> string -> value

  (* [get t s] takes in a database [t] and the SHA-256 hash [s] of a value, and 
   * returns the an option for value with that hash. *)
  val get_opt : value t -> string -> value option

  (* [create s] makes an object representing the database from the location of the
   * database [s]. *)
  val create : string -> value t

  (* [mem t s] is true if the value with SHA-256 [s] is in the database [t], and
   * false otherwise. *)
  val mem : t -> string -> bool Lwt.t

  (* [remove t s] removes the value with hash [s] from the database [t]. *)
  val remove : t -> string -> unit Lwt.t

end

module type Database(Hashable a)
