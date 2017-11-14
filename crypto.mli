(* [pubkey] is an ECDSA public key, represented as a point on
 * the curve secp256k1 *)
type pubkey

(* [privkey] is an ECDSA private key, a random 256-bit integer *)
type privkey

(*A pair of associated public/private keys *)
type keypair = (pubkey * privkey)

(* An ECDSA signature, plus recovery bit *)
type signature

(* [sha256 s] is the [sha256] hash function applied to [s] *)
val sha256 : string -> string

(* [scrypt salt s] is the password [s] stretched to 256 bits via the Scrypt kdf
 * with parameters n=16384, r=8, p=1 and [salt] as a salt. *)
val scrypt : string -> string -> string

(* [aes key iv msg] is [msg] encrypted with aes-256 in CBC mode with [iv] as the iv
 * and [key] as the encryption key. For security [key] should have been stretched with a
 * memory-hard key derivation function. *)
val aes : string -> string -> string -> string

(* [random i] is a cryptographic random byte string of length [i]
 * from the OS randomness source *)
val random : int -> string

(* [keypair d] is the ECDSA keypair with private key [d] *)
val keypair : string -> keypair

(* [sign key s] creates an ECDSA signature of the message [s] with the keypair [key].
 * Gets a random nonce from the OS randomness source. *)
val sign : keypair -> string -> signature

(* [recover msg sig] is the public key that was used to produce the signature [sig]. *)
val recover : string -> signature

(* [verify pub msg sig] is true iff [sig] is a valid signature of [msg] by the
 * public key [pub] using secp256k1 with SHA256 *)
val verify : pubkey -> string -> signature -> bool
