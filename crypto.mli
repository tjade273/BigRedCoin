module ECDSA : sig
  (* [pubkey] is an ECDSA public key, represented as a point on
   * the curve secp256k1. *)
  type pubkey

  (* [privkey] is an ECDSA private key, a random 256-bit integer. *)
  type privkey

  (*A pair of associated public/private keys. *)
  type keypair = (pubkey * privkey)

  (* An ECDSA "recoverable" signature *)
  type signature

  (* [pf_hex s] is the keypair derived from the private key string [s]
   * Prerequisite: [s] is a 64-character long hexidecimal ASCII string
   * Raises: Invalid_key *)
  val of_hex : string -> keypair

  (* [create ()] generates a new random ECDSA keypair *)
  val create : unit -> keypair

  (* [sign key s] creates an ECDSA signature of the message [s] with the keypair
   * [key]. Gets a random nonce from the OS randomness source. *)
  val sign : keypair -> string -> signature

  (* [recover msg sig] is the public key that was used to produce the signature
   * [sig]. [None] on invalid signature. *)
  val recover : string -> signature -> pubkey option

  (* [verify addr msg sig] is true iff [sig] is a valid signature of [msg] by the
   * address [addr] using secp256k1 with SHA256. *)
  val verify : string -> string -> signature -> bool

  (* [to_address pkey] is the hex-encoded address derived from [pkey] *)
  val to_address : pubkey -> string

  (* [string_of_sig s] is the serialization of the signature [s] as a string *)
  val string_of_sig : signature -> string

  (* [sig_of_string s] is the decoded signature that stringifies to [s] *)
  val sig_of_string : string -> signature option
end

module AES : sig
  (* An encrypted public/private keypair and associated metadata *)
  type t

  (* [of_string s] is attempts to parse [s] into an encrypted account *)
  val of_string : string -> t option

  (* [to_string s] is the string such that [read (write t)] is [t] *)
  val to_string : t -> string

  (* [decrypt t pswd] is the decrypted ECDSA keypair represented by [t], or
   * [None] if the password is incorrect *)
  val decrypt : t -> string -> ECDSA.keypair option

  (* [encrypt key pswd] is the encrypted form of [key] using the password [paswd] *)
  val encrypt : ECDSA.keypair -> string -> t

  (* [address a] is the address associated with the encrypted account [a] *)
  val address : t -> string
end

(* [random i] is a cryptographic random byte string of length [i]
 * from the OS randomness source. *)
val random : int -> string

(* [sha256 s] is the [sha256] hash function applied to [s]. *)
val sha256 : string -> string
