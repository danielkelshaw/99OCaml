(** Problem 16: Drop every N'th element from a list.*)

let drop (list : 'a list) (n : int) : 'a list =
  let rec _drop cnt list =
    match list with
    | [] -> []
    | hd :: tl -> if cnt = 1 then _drop n tl
                  else hd :: _drop (cnt - 1) tl
  in
  _drop n list

