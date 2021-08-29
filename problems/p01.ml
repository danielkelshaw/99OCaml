(** Problem 01: Write a function [last : 'a list  -> 'a option] that returns
                the last element of a list. *)

let rec last (list : 'a list) : 'a option =
  match list with
  | [] -> None
  | [e] -> Some e
  | hd :: tl -> last tl

