(** Problem 09: Pack consecutive duplicates of list elements into sublists.*)

let pack (list : 'a list) : 'a list list =
  let rec _pack acc curr_acc list =
    match list with
    | [] -> []
    | [x] -> (x :: curr_acc) :: acc
    | a :: (b :: _ as tl) ->
        if a = b then _pack acc (a :: curr_acc) tl
        else _pack ((a :: curr_acc) :: acc) [] tl
  in
  List.rev (_pack [] [] list)

