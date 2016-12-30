open Rectangle;;
open Pquadtree;;
open Rquadtree;;
open RCquadtree;;
open Graphics;;

type config = {base_length:int; scale:int;};;

let () =
  (* print_int (Foo.add 3 2);;
  print_string "hello :)";; *)
  print_string "*** Begining ***";;
  print_newline();;

  let config = {base_length=512; scale=2};;


  (* let pqt = (pinsert {x=300; y=10} (pinsert {x=30; y=30} PEmpty));; *)

  (* let pqt = pinsert_list PEmpty [
    {x=300; y=10}; {x=373; y=120}; {x=76; y=453}; {x=201; y=89};
    {x=35; y=225}; {x=242; y=29}; {x=294; y=382}; {x=298; y=24};
    {x=455; y=202}; {x=292; y=292}; {x=293; y=509}; {x=132; y=395};
  ];; *)

  (* let pqt = pinsert_list PEmpty [
    {x=100; y=100}; {x=120; y=120}; {x=140; y=140}; {x=160; y=160}; {x=180; y=180}; {x=200; y=200}; {x=220; y=220}; {x=240; y=240};
  ];; *)

  (* let pqt = pinsert_list PEmpty [
    {x=200; y=200}; {x=190; y=190};
  ];; *)

  (* let pqt_order_1 = pinsert_list PEmpty [
     {x=300; y=10}; {x=373; y=120}; {x=76; y=453}; {x=201; y=89};
   ];;
  let pqt_order_2 = pinsert_list PEmpty [
    {x=300; y=10}; {x=373; y=120}; {x=201; y=89}; {x=76; y=453};
  ];; *)

  (* let rqt = RQ (
    Plain White,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );; *)


  (* let rqt1 = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );;

  let rqt2 = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain White),
    Plain White
  );; *)

  (* let qt = RCNode(
    {top=512; bottom=0; left=0; right=512},
    [{top=100; bottom=10; left=100; right=300}],
    [],
    RCEmpty,
    RCEmpty,
    RCEmpty,
    RCEmpty
  );; *)

  let qt = rcinsert {top=100; bottom=10; left=100; right=200} RCEmpty;;


  let screen_size = Printf.sprintf " %dx%d"
    ((config.base_length+20)*config.scale) ((config.base_length+20)*config.scale);;

  try
    Graphics.open_graph screen_size;
    Graphics.set_window_title "Quadtrees";
    Graphics.set_line_width config.scale;

    (* draw_pquadtree config.scale pqt_order_2; *)
    (* draw_rquadtree config.scale config.base_length rqt; *)
    (* draw_rquadtree config.scale config.base_length (invert rqt); *)
    (* draw_rquadtree config.scale config.base_length (intersection rqt1 rqt2); *)

    (* draw_rquadtree config.scale config.base_length rqt1; *)
    (* draw_rquadtree config.scale config.base_length (horizontal_symmetry rqt1); *)

    draw_rcquadtree config.scale qt;

    ignore (Graphics.read_key ());
  with
  | Graphic_failure("fatal I/O error") -> print_string "Fermeture de la fenêtre" ; print_newline();;
