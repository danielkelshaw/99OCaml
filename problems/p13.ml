(** Problem 13: Run-length encoding of a list (direct solution).*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode (list : 'a list) : 'a rle list =
  let rle count x = if count = 0 then One x
                    else Many (count + 1, x) in
  let rec _encode acc count list =
    match list with
    | [] -> []
    | [x] -> rle count x :: acc
    | a :: (b :: _ as tl) -> if a = b then _encode acc (count + 1) tl
                             else _encode (rle count a :: acc) 0 tl
  in
  List.rev (_encode [] 0 list)

