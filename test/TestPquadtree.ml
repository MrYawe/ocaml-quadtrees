open OUnit2;;
open Rectangle;;
open Pquadtree;;


(******************************************************************************)
(*                             Test pbelong                                   *)
(******************************************************************************)

let simple_pquadtree = PNode (
  {x=10; y=10},
  {top=16; right=16; bottom=0; left=0},
  PEmpty, PEmpty, PEmpty, PEmpty
);;

let test_pbelong test_ctxt =
  assert_equal (pbelong {x=10; y=10} simple_pquadtree) true;;


(******************************************************************************)
(*                             Test pinsert                                   *)
(******************************************************************************)

let pqdt2 = (pinsert (pinsert PEmpty {x=30; y=30}) {x=300; y=10});;
let pqdt2_result =
PNode (
  {x=30; y=30},
  {top=base_length; right=base_length; bottom=0; left=0},
  PEmpty, PEmpty, PEmpty, PNode (
    {x=300; y=10},
    {top=(base_length/2); right=base_length; bottom=0; left=(base_length/2)},
    PEmpty, PEmpty, PEmpty, PEmpty
  )
);;

let test_pinsert test_ctxt =
  assert_equal pqdt2 pqdt2_result;;


(******************************************************************************)
(*                             Test order                                     *)
(* Proves that the shape of a paquadtree depend of the of the point's         *)
(* pinsertion order.                                                          *)
(******************************************************************************)

let pqt_order_11 = pinsert_list [
  {x=300; y=10}; {x=373; y=120}; {x=76; y=453}; {x=201; y=89};
];;
let pqt_order_12 = pinsert_list [
  {x=300; y=10}; {x=373; y=120}; {x=201; y=89}; {x=76; y=453};
];;

let pqt_order_21 = pinsert_list [
  {x=400; y=40}; {x=40; y=40}; {x=20; y=20};
];;
let pqt_order_22 = pinsert_list [
  {x=400; y=40}; {x=20; y=20}; {x=40; y=40};
];;

let test_order test_ctxt =
  (* It can be equals ... *)
  assert_equal pqt_order_11 pqt_order_12;
  (* but not all the time ... *)
  assert_equal (pqt_order_21=pqt_order_22) false;;

(* Name the test cases and group them together *)
let tests =
"tests">::: [
  "test_pbelong">:: test_pbelong;
  "test_pinsert">:: test_pinsert;
  "test_order">:: test_order;
];;
