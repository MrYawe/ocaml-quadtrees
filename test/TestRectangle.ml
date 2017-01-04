open OUnit2;;
open Point;;
open Rectangle;;

(******************************************************************************)
(*                             Test center                                    *)
(******************************************************************************)

let test_center test_ctxt =
  assert_equal (center {top=2; right=2; bottom=0; left=0}) {x=1; y=1};;
  (* assert_equal (1/2)
  assert_equal
    ~printer:string_of_point
    (center {top=4; right=4; bottom=3; left=3}) {x=1; y=1};; *)

let tests =
"tests">::: [
  "test_center">:: test_center;
];;
