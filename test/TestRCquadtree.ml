open OUnit2;;
open Rectangle;;
open RCquadtree;;

(******************************************************************************)
(*                            Test rcinsert                                   *)
(******************************************************************************)

(* alias *)
let bs = base_surface;;

let rcqt1 = rcinsert {top=100; bottom=10; left=100; right=300} RCEmpty;;
let rcqt2 = rcinsert {top=100; bottom=10; left=100; right=200} RCEmpty;;

let rcqt1_rcinsert_res = RCNode(
  bs,
  [{top=100; bottom=10; left=100; right=300}], [],
  RCEmpty, RCEmpty, RCEmpty, RCEmpty
);;
let rcqt2_rcinsert_res = RCNode(
  bs,
  [], [],
  RCEmpty,
  RCEmpty,
  RCNode(
    {top=(bs.top/2); bottom=bs.bottom; left=bs.left; right=(bs.right/2)},
    [{top=100; bottom=10; left=100; right=200}], [],
    RCEmpty, RCEmpty, RCEmpty, RCEmpty
  ),
  RCEmpty
);;

let test_rcinsert test_ctxt =
  assert_equal (rcquadtree_equal rcqt1 rcqt1_rcinsert_res) true;
  assert_equal (rcquadtree_equal rcqt2 rcqt2_rcinsert_res) true;;


(******************************************************************************)
(*                            Test contains                                   *)
(******************************************************************************)

(* let test_rects_contain test_ctxt =
  assert_equal (rcquadtree_equal rcqt1 rcqt1_rcinsert_res) true;
  assert_equal (rcquadtree_equal rcqt2 rcqt2_rcinsert_res) true;; *)



let tests =
"tests">::: [
  "test_rcinsert">:: test_rcinsert;
];;
