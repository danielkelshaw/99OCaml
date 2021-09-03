(** Problem 36: Determine the prime factors of a given positive integer (2).*)

let factors (n : int) : (int * int) list =
  let rec aux d n =
    if n = 1 then []
    else
      if n mod d = 0 then
        match aux d (n / d) with
        | (h, n) :: tl when h = d -> (h, n + 1) :: tl
        | l -> (d, 1) :: l
      else aux (d + 1) n
  in
  aux 2 n

