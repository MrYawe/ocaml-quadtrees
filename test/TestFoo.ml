open OUnit2;;

let test1 test_ctxt = assert_equal "x" (Foo.unity "x");;

let test2 test_ctxt = assert_equal 100 (Foo.unity 100);;

(* Name the test cases and group them together *)
let tests =
"tests">::: [
  "test1">:: test1;
  "test2">:: test2
];;
