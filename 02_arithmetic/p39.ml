(** Problem 39: A list of prime numbers.*)

let is_prime (n : int) : bool =
  let n = abs n in
  let rec not_factor d =
    d * d > n || (n mod d <> 0 && not_factor (d + 1))
  in
    not_factor 2

let rec all_primes (a : int) (b : int) : int list =
  if a > b then []
  else
    let rest = all_primes (a + 1) b in
    if is_prime a then a :: rest
    else rest

