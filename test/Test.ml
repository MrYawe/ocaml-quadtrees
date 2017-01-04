open OUnit2;;


let tests = [
  "TESTS POINT">::: [
    TestPoint.tests;
  ];
  "TESTS RECTANGLE">::: [
    TestRectangle.tests;
  ];
  "TESTS PQUADTREE">::: [
    TestPquadtree.tests;
  ];
  "TESTS RQUADTREE">::: [
    TestRquadtree.tests;
  ];
  "TESTS RCQUADTREE">::: [
    TestRCquadtree.tests;
  ];
];;

let () =
  List.iter run_test_tt_main tests;
;;
