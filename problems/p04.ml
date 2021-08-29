(** Find the number of elements of a list.*)

let length (list : 'a list) : int =
  let rec _length acc list =
    match list with
    | [] -> acc
    | hd :: tl -> _length (acc + 1) tl
  in
  _length 0 list

