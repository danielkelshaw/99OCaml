(** Problem 08: Eliminate consecutive duplicates of list elements.*)

let rec compress (list : 'a list) : 'a list =
  match list with
  | a :: (b :: _ as tl) -> if a = b then compress tl else a :: compress tl
  | elem -> elem

