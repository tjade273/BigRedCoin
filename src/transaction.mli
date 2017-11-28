(* The output of a transaction. Amount is how much is sent to an address and
 * address is the recipient of a transaction. *)
type output = {
  amount: int;
  address : string
}

(* The input to a transaction. txid is the SHA-256 hash of the transaction the
 * input comes from and out_index is the index of the input. The signature is
 * the ECDSA signature of the hash of the transaction id, out_index, and list
 * of outputs from the transaction.*)
type input = {
  txid : string;
  out_index : int;
  signature : string
}

(* A transaction is a list of outputs to which coins are sent and a list of
 * inputs from which coins originate. The sum of the amounts in the outputs
 * linked to the inputs must sum to the sum of the outputs of this transaction.
 * *)
type transaction = {
  outs : output list;
  ins : input list
}

(* [merkle_root [tx1; ...; txn]] is the root of the binary tree generated by
 * pairwise hashing the transactions. *)
val merkle_root : transaction list -> string