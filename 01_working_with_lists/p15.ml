(** Problem 15: Replicate the elements of a list a given number of times.*)

let replicate (list : 'a list) (n : int) : 'a list =
  let rec prepend n acc x =
    if n = 0 then acc
    else prepend (n - 1) (x :: acc) x in
  let rec _replicate acc list =
    match list with
    | [] -> acc
    | hd :: tl -> _replicate (prepend n acc hd) tl
  in
  List.rev (_replicate [] list)

