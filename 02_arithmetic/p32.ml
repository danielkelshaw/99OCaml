(** Problem 32: Determine the greatest common divisor of two positive
                integer numbers.*)

let rec gcd (a : int) (b : int) : int =
  if b = 0 then a
  else gcd b (a mod b)

