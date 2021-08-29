(** Problem 10: Run-length encoding of a list.*)

let encode (list : 'a list) : (int * 'a) list =
  let rec _encode count acc list =
    match list with
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as tl) -> if a = b then _encode (count + 1) acc tl
                             else _encode 0 ((count + 1, a) :: acc) tl
  in
  List.rev (_encode 0 [] list)
 
