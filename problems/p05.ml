(** Problem 05: Reverse a list.*)

let rev (list : 'a list) : 'a list =
  let rec _rev acc list =
    match list with
    | [] -> acc
    | hd :: tl -> _rev (hd :: acc) tl
  in
  _rev [] list

