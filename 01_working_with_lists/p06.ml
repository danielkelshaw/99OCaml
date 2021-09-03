(** Problem 06: Find out whether  list is a palindrome.*)

let is_palindrome (list : 'a  list) : bool =
  list = P05.rev list

