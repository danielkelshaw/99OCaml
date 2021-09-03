(** Problem 31: Determine whether a given integer number is prime.*)

let is_prime (n : int) : bool =
  let n = abs n in
  let rec not_factor d =
    d * d > n || (n mod d <> 0 && not_factor (d + 1))
  in
  n <> 1 && not_factor 2

