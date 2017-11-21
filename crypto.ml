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
  type keypair = privkey * pubkey

  type signature = RecoverableSign.t

  let hash_msg msg = msg
                   |> Cstruct.of_string
                   |> Nocrypto.Hash.SHA256.digest
                   |> Cstruct.to_bigarray

  let to_address pubkey =
    let pubkey_hash = pubkey
                      |> Secp256k1.Public.to_bytes context
                      |> Cstruct.of_bigarray
                      |> Nocrypto.Hash.SHA256.digest
    in
    let (addr, _) = Cstruct.split pubkey_hash 20 in
    Cstruct.to_string addr

  let of_hex s =
    let buf = s |> Hex.of_string |> Hex.to_cstruct |> Cstruct.to_bigarray in
    Secret.of_bytes_exn context buf

  let sign (pub, priv) msg =
    RecoverableSign.sign context ~seckey:priv (hash_msg msg)

  let recover msg signature =
    RecoverableSign.recover context signature (hash_msg msg)

  let verify address msg signature =
    to_address (recover msg signature) = address
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
      if Cstruct.len iv = 32 &&
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

  let scrypt salt pswd =
  Scrypt_kdf.scrypt_kdf
    ~password:pswd
    ~salt:salt
    ~n:16384 ~r:8 ~p:1 ~dk_len:(Int32.of_int 32)

  let decrypt {address; iv; salt; ciphertext} pswd =
    let stretched = scrypt salt pswd in
    let key = of_secret stretched in
    let plaintext= decrypt ~key:key ~iv:iv ciphertext |> Cstruct.to_bigarray in
    try
      let privkey = Secp256k1.Secret.of_bytes_exn context plaintext in
      let pubkey = Secp256k1.Public.of_secret context privkey in
      if ECDSA.to_address pubkey = address
      then Some (pubkey, privkey)
      else None
    with Invalid_argument _ -> None

  let encrypt (pubkey, privkey) pswd =
    let salt = Nocrypto.Rng.generate 32 in
    let stretched = scrypt salt pswd in
    let iv = Nocrypto.Rng.generate 32 in
    let plaintext = Secp256k1.Secret.to_bytes privkey |> Cstruct.of_bigarray in
    let address = ECDSA.to_address pubkey in
    let ciphertext = encrypt ~key:(of_secret stretched) ~iv:iv plaintext in
    {address; iv; salt; ciphertext}

  let address {address; _} = address
end
