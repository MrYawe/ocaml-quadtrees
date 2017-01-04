open OUnit2;;

let () =

  run_test_tt_main
    ("TESTS">::: [
      TestPoint.tests;
      TestRectangle.tests;
      TestPquadtree.tests;
      TestRquadtree.tests;
      TestRCquadtree.tests;
   ]);
;;
