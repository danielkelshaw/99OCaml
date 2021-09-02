(** Problem 19: Rotate a list N places to the left.*)

let split (list : 'a list) (n : int) : 'a list * 'a list =
  let rec aux i acc list =
    match list with
    | [] -> List.rev acc, []
    | hd :: tl as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (hd :: acc) tl
  in
  aux n [] list

let rotate (list : 'a list) (n : int) : 'a list =
  let len = List.length list in
  let n = if len = 0 then 0
          else (n mod len + len) mod len in
  if n = 0 then list
  else let a, b = split list n in b @ a

