(** Problem 22: Create a list containing all integers within a given range.*)

let range (a : int) (b : int) : int list =
  let rec aux a b =
    if a > b then []
    else a :: aux (a + 1) b
  in
    if a > b then List.rev (aux b a)
    else aux a b

