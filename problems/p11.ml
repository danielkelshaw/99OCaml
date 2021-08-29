(** Problem 11: Modified run-length encoding.*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode (list : 'a list) : 'a rle list =
  let rec create_tuple cnt elem =
    if cnt = 1 then One elem
    else Many (cnt, elem) in
  let rec _encode count acc list =
    match list with
    | [] -> []
    | [x] -> (create_tuple (count + 1) x) :: acc
    | a :: (b :: _ as tl) ->
        if a = b then _encode (count + 1) acc tl
        else _encode 0 ((create_tuple (count + 1) a) :: acc) tl
  in
  List.rev (_encode 0 [] list)

