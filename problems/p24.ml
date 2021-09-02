(** Problem 24: Lotto: Draw N different random numbers from the set 1..M.*)

let lotto_select (n : int) (m : int) : int list =
  P23.rand_select (P22.range 1 m) n

