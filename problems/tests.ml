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

let test_p08 =
  "P08" >::: [
    "T1" >:: (fun _ -> assert_equal [`a; `b; `c; `a; `d; `e] 
                                    (P08.compress [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `e; `e; `e; `e]))
  ]

let test_p09 =
  "P09" >::: [
    "T1" >:: (fun _ -> assert_equal [[`a; `a; `a; `a]; [`b]; [`c; `c]; [`a; `a]; [`d; `d]; [`e; `e; `e; `e]]
                                    (P09.pack [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `d; `e; `e; `e; `e]))
  ]

let test_p10 =
  "P10" >::: [
    "T1" >:: (fun _ -> assert_equal [(4, `a); (1, `b); (2, `c); (2, `a); (2, `d); (4, `e)]
                                    (P10.encode [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `d; `e; `e; `e; `e]))
  ]

open P11
let test_p11 =
  "P11" >::: [
    "T1" >:: (fun _ -> assert_equal [Many (4, `a); One `b; Many (2, `c); Many (2, `a); One `d; Many (4, `e)]
                                    (P11.encode [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `e; `e; `e; `e]))
  ]

open P12
let test_p12 =
  "P12" >::: [
    "T1" >:: (fun _ -> assert_equal [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `e; `e; `e; `e]
                                    (P12.decode [Many (4, `a); One `b; Many (2, `c); Many (2, `a); One `d; Many (4, `e)]))
  ]

open P13
let test_p13 =
  "P13" >::: [
    "T1" >:: (fun _ -> assert_equal [Many (4, `a); One `b; Many (2, `c); Many (2, `a); One `d; Many (4, `e)]
                                    (P13.encode [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `e; `e; `e; `e]))
  ]

let test_p14 =
  "P14" >::: [
    "T1" >:: (fun _ -> assert_equal [`a; `a; `b; `b; `c; `c; `c; `c; `d; `d]
                                    (P14.duplicate [`a; `b; `c; `c; `d]))
  ]

let test_p15 =
  "P15" >::: [
    "T1" >:: (fun _ -> assert_equal [`a; `a; `a; `b; `b; `b; `c; `c; `c]
                                    (P15.replicate [`a; `b; `c] 3))
  ]

let test_p16 =
  "P16" >::: [
    "T1" >:: (fun _ -> assert_equal [`a; `b; `d; `e; `g; `h; `j]
                                    (P16.drop [`a; `b; `c; `d; `e; `f; `g; `h; `i; `j] 3))
  ]

let test_p17 =
  "P17" >::: [
    "T1" >:: (fun _ -> assert_equal ([`a; `b; `c], [`d; `e; `f; `g; `h; `i; `j])
                                    (P17.split [`a; `b; `c; `d; `e; `f; `g; `h; `i; `j] 3));
    "T2" >:: (fun _ -> assert_equal ([`a; `b; `c; `d], [])
                                    (P17.split [`a; `b; `c; `d] 5))
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
      test_p08;
      test_p09;
      test_p10;
      test_p11;
      test_p12;
      test_p13;
      test_p14;
      test_p15;
      test_p16;
      test_p17;
    ]

let _ = 
  run_test_tt_main test_suite

