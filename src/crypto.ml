let () = Nocrypto_entropy_unix.initialize ()

let context =
  let ctxt = Secp256k1.Context.(create [Sign; Verify]) in
  let rand = Cstruct.to_bigarray (Nocrypto.Rng.generate 32) in
  ignore @@ Secp256k1.Context.randomize ctxt rand; ctxt

let random len =
  Nocrypto.Rng.generate len
  |> Cstruct.to_string

let sha256 msg = msg
                 |> Cstruct.of_string
                 |> Nocrypto.Hash.SHA256.digest
                 |> Cstruct.to_string

module ECDSA = struct
  open Secp256k1

  type pubkey = Public.t
  type privkey = Secret.t
  type keypair =  pubkey * privkey

  type signature = RecoverableSign.t

(* [hash_msg msg] is a [bigarray] resulting from applying the [sha256] hash
 * function to [msg]. *)
  let hash_msg msg = msg
                     |> Cstruct.of_string
                     |> Nocrypto.Hash.SHA256.digest
                     |> Cstruct.to_bigarray

  let to_address pubkey =
    let pubkey_hash = pubkey
                      |> Secp256k1.Public.to_bytes ~compress:false context
                      |> Cstruct.of_bigarray
                      |> fun s -> Cstruct.shift s 1
                      |> Nocrypto.Hash.SHA256.digest
    in
    let (addr, _) = Cstruct.split pubkey_hash 20 in
    Hex.of_cstruct addr |> Hex.show

  let rec create () =
    let prv = Nocrypto.Rng.generate 32 |> Cstruct.to_bigarray in
    let secret = Secret.read context prv in
    match secret with
    | None -> create ()
    | Some s -> (Public.of_secret context s, s)

  let of_hex s =
    let buf = `Hex s |> Hex.to_cstruct |> Cstruct.to_bigarray in
    let privkey = Secret.read_exn context buf in
    let pubkey = Public.of_secret context privkey in
    (pubkey, privkey)

  let sign (pub, priv) msg =
    RecoverableSign.sign context ~seckey:priv (hash_msg msg)

  let recover msg signature =
    RecoverableSign.recover context signature (hash_msg msg)

  let verify address msg signature =
    match recover msg signature with
    | None -> false
    | Some pubkey -> to_address pubkey = address

  let string_of_sig s =
    let (s, v) = RecoverableSign.to_compact context s in
    let buf = Cstruct.create 65 in
    Cstruct.blit (Cstruct.of_bigarray s) 0 buf 0 64;
    Cstruct.set_uint8 buf 64 v; Cstruct.to_string buf

  let sig_of_string s =
    try
      let b = Cstruct.of_string s in
      let v = Cstruct.get_uint8 b 64 in
      if v >= 0 && v <= 3 then
        RecoverableSign.of_compact ~recid:v context (Cstruct.to_bigarray b)
      else None
    with Invalid_argument _ | Unix.Unix_error _ -> None
end

module AES = struct

  open Nocrypto.Cipher_block.AES.CBC

  type t = {address : string;
            iv : Cstruct.t;
            salt : Cstruct.t;
            ciphertext : Cstruct.t}

  let of_string s =
    let open Yojson.Basic in
    let from_hex s =  `Hex(Util.to_string s) |> Hex.to_cstruct in
    try
      let json = from_string s in
      let address  = Util.member "address" json |> Util.to_string in
      let iv = Util.member "IV" json |> from_hex in
      let salt = Util.member "salt" json |> from_hex in
      let ciphertext = Util.member "private key" json |> from_hex in
      if Cstruct.len iv = 16 &&
         Cstruct.len salt = 32 &&
         String.length address = 40
      then Some {address; iv; salt; ciphertext} else None
    with
    | Util.Type_error _ | Util.Undefined _ | Invalid_argument _ -> None

  let to_string {address; iv; salt; ciphertext} =
    let to_hex b = `String(b |> Hex.of_cstruct |> Hex.show) in
    let json = `Assoc [
        ("address", `String address);
        ("IV", to_hex iv);
        ("salt", to_hex salt);
        ("private key", to_hex ciphertext)]
    in
    Yojson.Basic.to_string json

(* [scrypt salt password] is a scrypt derived key for [password] using the params
 * salt=[salt], n=16348, r=8, p=1, dklen=32. *)
  let scrypt salt password =
    let pswd = Cstruct.of_string password in
    Scrypt_kdf.scrypt_kdf
      ~password:pswd
      ~salt:salt
      ~n:16384 ~r:8 ~p:1 ~dk_len:(Int32.of_int 32)

  let decrypt {address; iv; salt; ciphertext} password =
    let stretched = scrypt salt password in
    let key = of_secret stretched in
    let plaintext= decrypt ~key:key ~iv:iv ciphertext |> Cstruct.to_bigarray in
    try
      let privkey = Secp256k1.Secret.read_exn context plaintext in
      let pubkey = Secp256k1.Public.of_secret context privkey in
      if ECDSA.to_address pubkey = address
      then Some (pubkey, privkey)
      else None
    with Invalid_argument _ | Failure _ -> None

  let encrypt (pubkey, privkey) pswd =
    let salt = Nocrypto.Rng.generate 32 in
    let stretched = scrypt salt pswd in
    let iv = Nocrypto.Rng.generate 16 in
    let plaintext = Secp256k1.Secret.to_bytes privkey |> Cstruct.of_bigarray in
    let address = ECDSA.to_address pubkey in
    let ciphertext = encrypt ~key:(of_secret stretched) ~iv:iv plaintext in
    {address; iv; salt; ciphertext}

  let address {address; _} = address
end
