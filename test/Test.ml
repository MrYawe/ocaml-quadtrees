open OUnit2;;

let () =

  run_test_tt_main
    ("TESTS">::: [
      TestFoo.tests;
      TestPquadtree.tests;
   ]);
;;
