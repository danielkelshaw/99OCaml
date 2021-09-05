open OUnit2
open Batteries


open P61
let example_tree = Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)), Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
let test_p61 =
  "P61" >::: [
    "T1" >:: (fun _ -> assert_equal 0 (P61.count_leaves Empty));
    "T2" >:: (fun _ -> assert_equal 3 (P61.count_leaves example_tree))
  ]
 
let test_p61a =
  "P61a" >::: [
    "T1" >:: (fun _ -> assert_equal [] (P61.leaves Empty));
    "T2" >:: (fun _ -> assert_equal ['d'; 'e'; 'g'] (P61.leaves example_tree))
  ]

open P62
let example_tree = Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)), Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
let test_p62 =
  "P62" >::: [
    "T1" >:: (fun _ -> assert_equal [] (P62.internals Empty));
    "T2" >:: (fun _ -> assert_equal ['b'; 'a'; 'c'; 'f'] (P62.internals example_tree))
  ]

let test_p62b =
  "P62b" >::: [
    "T1" >:: (fun _ -> assert_equal ['b'; 'c'] (P62.at_level example_tree 2));
    "T2" >:: (fun _ -> assert_equal [] (P62.at_level example_tree 5))
  ]

let test_suite =
  "Problems" >:::
    [
      test_p61;
      test_p61a;
      test_p62;
      test_p62b;
    ]

let _ = 
  run_test_tt_main test_suite

