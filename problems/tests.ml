open OUnit2

let test_p01 =
  "P01" >::: [
    "T1" >:: (fun _ -> assert_equal (Some `d) (P01.last [`a; `b; `c; `d]));
    "T2" >:: (fun _ -> assert_equal None (P01.last []))
  ]

let test_p02 =
  "P02" >::: [
    "T1" >:: (fun _ -> assert_equal (Some (`c, `d)) (P02.last_two [`a; `b; `c; `d]));
    "T2" >:: (fun _ -> assert_equal None (P02.last_two [`a]))
  ]

let test_p03 =
  "P03" >::: [
    "T1" >:: (fun _ -> assert_equal (Some `c) (P03.at 3 [`a; `b; `c; `d]));
    "T2" >:: (fun _ -> assert_equal None (P03.at 3 [`a]))
  ]

let test_p04 =
  "P04" >::: [
    "T1" >:: (fun _ -> assert_equal 3 (P04.length [`a; `b; `c]));
    "T2" >:: (fun _ -> assert_equal 0 (P04.length []))
  ]

let test_p05 =
  "P05" >::: [
    "T1" >:: (fun _ -> assert_equal [`c; `b; `a] (P05.rev [`a; `b; `c]))
  ]

let test_p06 =
  "P06" >::: [
    "T1" >:: (fun _ -> assert_equal true (P06.is_palindrome [`x; `a; `m; `a; `x]));
    "T2" >:: (fun _ -> assert_equal false (P06.is_palindrome [`a; `b]))
  ]

open P07
let test_p07 =
  "P07" >::: [
    "T1" >:: (fun _ -> assert_equal [`a; `b; `c; `d; `e]
                                    (P07.flatten [One `a; Many [One `b; Many [One `c; One `d]; One `e]]))
  ]

let test_suite =
  "Problems" >:::
    [
      test_p01;
      test_p02;
      test_p03;
      test_p04;
      test_p05;
      test_p06;
      test_p07;
    ]

let _ = 
  run_test_tt_main test_suite

