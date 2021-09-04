(** Problem 40: Goldbach's conjecture.*)

let goldbach (n : int) : int * int =
  let rec aux d =
    if P31.is_prime d && P31.is_prime (n - d) then (d, n - d)
    else aux (d + 1)
  in
    aux 2

