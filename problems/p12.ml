(** Problem 12: Decode a run-length encoded list,*)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode (list : 'a rle list) : 'a list =
  let rec many acc cnt x =
    if cnt = 0 then acc
    else many (x :: acc) (cnt - 1) x
  in
  let rec _decode acc list = 
    match list with
    | [] -> acc
    | One x :: tl -> _decode (x :: acc) tl
    | Many (cnt, x) :: tl -> _decode (many acc cnt x) tl
  in
  List.rev (_decode [] list)

