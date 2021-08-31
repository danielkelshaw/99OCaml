(** Problem 17: Split a list into two parts; the length of the first part
                is given.*)

let split (list : 'a list) (n : int) : 'a list * 'a list =
  let rec _split acc cnt list =
    match list with
    | [] -> List.rev acc, []
    | hd :: tl as l-> if cnt = 0 then List.rev acc, l
                      else _split (hd :: acc) (cnt - 1) tl
  in
  _split [] n list

