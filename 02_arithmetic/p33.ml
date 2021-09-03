(** Problem 33: Determine whether two positive integer numbers are coprime.*)

let coprime (a : int) (b : int) : bool =
  P32.gcd a b = 1

