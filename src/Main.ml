open Point;;
open Rectangle;;
open Pquadtree;;
open Rquadtree;;
open RCquadtree;;
open Graphics;;

type config = { base_length:int; scale:int; };;
type state = { mutable pos:int; max_pos:int; };;

exception End;;


let pquadtree_1 () =
  let pqt = (pinsert (pinsert PEmpty {x=30; y=30}) {x=300; y=10}) in
    draw_pquadtree pqt;;

let pquadtree_2 () =
  let pqt = pinsert_list [
    {x=300; y=10}; {x=373; y=120}; {x=76; y=453}; {x=201; y=89};
    {x=35; y=225}; {x=242; y=29}; {x=294; y=382}; {x=298; y=24};
    {x=455; y=202}; {x=292; y=292}; {x=293; y=509}; {x=132; y=395};
  ] in draw_pquadtree pqt;;

let demo_list = [
  pquadtree_1;
  pquadtree_2;
]





let skel f_init f_end f_key (*f_mouse*) f_except =
  f_init ();
  try
    while true do
      try
        let s = wait_next_event
          [Key_pressed]
        in if s.keypressed then f_key s.key
          (* else if s.button
            then f_mouse s.mouse_x s.mouse_y *)
      with
        | End -> raise End
        | e -> f_except e
    done
  with
    End -> f_end ();;

let f_init state () =
  let config = {base_length=512; scale=2} in
  let screen_size = Printf.sprintf " %dx%d"
    ((config.base_length+20)*config.scale)
    ((config.base_length+20)*config.scale) in

  Graphics.open_graph screen_size;
  Graphics.set_window_title "Quadtrees";
  Graphics.set_line_width config.scale;
  (List.nth demo_list state.pos) ();;

let f_end state () =
  print_string "end";;

(* let g_next state =
  print_string "next ";
  if (state.pos+1) >= state.pos_max then
    state.pos <- (state.pos+1);
    print_string (string_of_int state.pos);
    print_string "\n";; *)

(* let prev () =
  print_string "prev ";
  if (state.pos-1) >= 0 then
    state.pos <- (state.pos-1);
    print_string (string_of_int state.pos);
    print_string "\n";; *)

let f_key state c =
  print_string (string_of_int state.pos);
  print_string " -> ";
  Graphics.clear_graph ();
  (match c with
    | '&' -> raise End
    | 'q' -> if state.pos > 0 then state.pos <- state.pos-1
    | 'd' -> if state.pos < state.max_pos then state.pos <- state.pos+1
    (* | '\n' -> next_line ()
    | '\r' -> next_line () *)
    | _ -> ());
    print_string (string_of_int state.pos);
    print_string "\n";
    (List.nth demo_list state.pos) ();;

let f_except state e =
  Printf.printf "%s\n" (Printexc.to_string e);;

let g_state = {pos=0; max_pos=(List.length demo_list)-1};;

let start () =
  skel (f_init g_state) (f_end g_state) (f_key g_state)
    (f_except g_state);;

let () =
  print_string "*** Begining ***";;
  print_newline();;

  start ();;


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

  (* let qt = rcinsert ~surface:{top=1024; right=1024; bottom=0; left=0}
            {top=100; bottom=10; left=100; right=300} RCEmpty;; *)

  (* let qt = rcinsert_list [
    {top=460; bottom=310; left=230; right=310};
    {top=265; bottom=230; left=300; right=480};
    {top=215; bottom=150; left=110; right=235};
    {top=490; bottom=460; left=410; right=485};
    {top=275; bottom=240; left=260; right=310};
    {top=470; bottom=460; left=2; right=20};
    {top=250; bottom=210; left=270; right=305};
  ];; *)

  (* print_string (string_of_rectangle_list [{top=460; bottom=310; left=230; right=310}; {top=460; bottom=310; left=230; right=310}]);; *)

  (* let res = rccontain qt {x=303; y=245};;
  print_string (string_of_rectangle_list res);; *)


  (* let pqt_order_21_bis = pinsert {x=76; y=453} (pinsert {x=201; y=89} (pinsert {x=373; y=120} (pinsert {x=300; y=10} PEmpty)));; *)

  let pqt_order_21 = pinsert_list [
    {x=300; y=10}; {x=373; y=120}; {x=201; y=89}; {x=76; y=453};
  ];;

  let pqt_order_22 = pinsert_list [
    {x=300; y=10}; {x=373; y=120}; {x=76; y=453};
  ];;


  (* print_string (string_of_pquadtree pqt_order_21_bis);;
  print_string "\n\n\n";; *)
  (* print_string (string_of_pquadtree pqt_order_21);;
  print_string "\n\n\n";;
  print_string (string_of_pquadtree pqt_order_22);; *)

  (* let screen_size = Printf.sprintf " %dx%d"
    ((config.base_length+20)*config.scale) ((config.base_length+20)*config.scale);;

  try
    Graphics.open_graph screen_size;
    Graphics.set_window_title "Quadtrees";
    Graphics.set_line_width config.scale;

    (* draw_pquadtree ~scale:config.scale pqt_order_2; *)
    (* draw_rquadtree ~scale:config.scale config.base_length rqt; *)
    (* draw_rquadtree ~scale:config.scale config.base_length (invert rqt); *)
    (* draw_rquadtree ~scale:config.scale config.base_length (intersection rqt1 rqt2); *)

    (* draw_rquadtree ~scale:config.scale config.base_length rqt1; *)
    (* draw_rquadtree ~scale:config.scale config.base_length (horizontal_symmetry rqt1); *)

    draw_pquadtree ~scale:config.scale pqt_order_21;

    ignore (Graphics.read_key ());
  with
  | Graphic_failure("fatal I/O error") -> print_string "Fermeture de la fenêtre" ; print_newline();; *)
