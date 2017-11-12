(* The header of a block in the chain. Version is the version of the protocol.
 * The prev_hash is the hash of the previous block. The merkle_root is the
 * root of this block in the merkle_tree. The nonce is as in the proof of work
 * algorithm. The timestamp is the seconds since the unix epoch when the block
 * was made. *)
type header = {
  version : int;
  prev_hash : string; 
  merkle_root : string; 
  nonce : int; 
  timestamp : int
}

(* The output of a transaction. Amount is how much is sent to an address and
 * address is the recipient of a transaction. *)
and output = {
  amount: int;
  address : address
}

(* The input to a transaction. txid is the SHA-256 hash of the transaction the
 * input comes from and out_index is the index of the input. The signature is
 * the ECDSA signature of the hash of the transaction id, out_index, and list
 * of outputs from the transaction.*)
and input = {
  txid : string;
  out_index : int;
  signature : string
}

(* A transaction is a list of outputs to which coins are sent and a list of 
 * inputs from which coins originate. The sum of the amounts in the outputs
 * linked to the inputs must sum to the sum of the outputs of this transaction.
 * *)
and transaction = {
  outs = output list
  ins = input list
}

(* A block consists of its header, the transactions in the block, and a count
 * of the number of transactions in the block. *)
and block = {
  header : header;
  transactions : transaction list;
  transactions_count : int
}
