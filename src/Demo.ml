open Point;;
open Rectangle;;
open Pquadtree;;
open Rquadtree;;
open RCquadtree;;
open Graphics;;
open String;;

type state = { base_length:int; base_surface:rect; g_origin:point; scale:int;
  margin:int; mutable pos:int; max_pos:int; };;

exception End;;

let new_state length scale margin =
{
  base_length=length;
  base_surface={top=length; right=length; bottom=0; left=0};
  g_origin={x=margin; y=margin};
  margin=margin;
  scale=scale;
  pos=0;
  max_pos=10;
};;

let g_state = new_state 512 2 100;;

let draw_window_title s title =
  set_window_title
    (Printf.sprintf "%s (%d/%d)" title (s.pos+1) (s.max_pos+1));;

let pquadtree_1 s () =
  let pqt =
    (pinsert
      (pinsert ~surface:s.base_surface PEmpty {x=234; y=100})
      {x=32; y=400})
      in
    draw_pquadtree
      ~g_origin:{x=406; y=s.g_origin.y} ~scale:s.scale pqt;
    draw_window_title s "Pquadtree 1";;

let pquadtree_2 s () =
  let pqt = pinsert_list ~surface:s.base_surface [
    {x=234; y=100}; {x=32; y=400}; {x=17; y=500};
  ] in
    draw_pquadtree ~scale:s.scale ~g_origin:{x=406; y=s.g_origin.y} pqt;
  draw_window_title s "Pquadtree 2";;

let pquadtree_3 s () =
  let pqt = pinsert_list ~surface:s.base_surface [
    {x=234; y=100}; {x=32; y=400}; {x=17; y=500}; {x=33; y=22};
  ] in
    draw_pquadtree ~scale:s.scale ~g_origin:{x=406; y=s.g_origin.y} pqt;
  draw_window_title s "Pquadtree 3";;

let pquadtree_4 s () =
  let pqt = pinsert_list ~surface:s.base_surface [
    {x=234; y=100}; {x=32; y=400}; {x=17; y=500}; {x=33; y=22}; {x=240; y=150};
  ] in
    draw_pquadtree ~scale:s.scale ~g_origin:{x=406; y=s.g_origin.y} pqt;
  draw_window_title s "Pquadtree 4";;

let pquadtree_5 s () =
  let pqt = pinsert_list ~surface:s.base_surface [
    {x=234; y=100}; {x=32; y=400}; {x=17; y=500}; {x=33; y=22}; {x=240; y=150};
    {x=274; y=100}; {x=324; y=235}; {x=23; y=382}; {x=50; y=320}; {x=20; y=150};
    {x=234; y=120}; {x=104; y=204}; {x=124; y=239}; {x=24; y=204}; {x=240; y=150};
    {x=14; y=130}; {x=32; y=303}; {x=214; y=392}; {x=33; y=22}; {x=240; y=244};
    {x=35; y=225}; {x=242; y=429}; {x=294; y=382}; {x=298; y=24}; {x=15; y=204};
    {x=292; y=292}; {x=293; y=509}; {x=132; y=395}; {x=284; y=125};
    {x=462; y=98}; {x=307; y=390}; {x=510; y=248};

  ] in
    draw_pquadtree ~scale:s.scale ~g_origin:{x=406; y=s.g_origin.y} pqt;
  draw_window_title s "Pquadtree 5";;

let rquadtree_invert s () =
  let qt = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  ) in
  draw_rquadtree ~scale:s.scale ~g_origin:s.g_origin qt;
  let res = invert qt in
    let g_origin2 = {x=s.g_origin.x+s.base_length+100; y=s.g_origin.y} in
      draw_rquadtree ~scale:s.scale ~g_origin:g_origin2 res;
    draw_window_title s "Rquadtree: invert";;

let rquadtree_intersection s () =
  let qt1 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain Black
  ) and
  qt2 = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain White),
    Plain White
  ) and surface = {top=256; right=256; bottom=0; left=0} in
    draw_rquadtree
      ~scale:s.scale ~g_origin:{x=396; y=s.g_origin.y+300} ~surface:surface qt1;
    draw_rquadtree
      ~scale:s.scale ~g_origin:{x=672; y=s.g_origin.y+300} ~surface:surface qt2;
  let res = intersection qt1 qt2 in
      draw_rquadtree ~scale:s.scale
        ~g_origin:{x=534; y=s.g_origin.y-50} ~surface:surface res;
  draw_window_title s "Rquadtree: intersection";;

let rquadtree_union s () =
  let qt1 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain Black
  ) and
  qt2 = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain White),
    Plain White
  ) and surface = {top=256; right=256; bottom=0; left=0} in
    draw_rquadtree
      ~scale:s.scale ~g_origin:{x=396; y=s.g_origin.y+300} ~surface:surface qt1;
    draw_rquadtree
      ~scale:s.scale ~g_origin:{x=672; y=s.g_origin.y+300} ~surface:surface qt2;
  let res = union qt1 qt2 in
      draw_rquadtree ~scale:s.scale
        ~g_origin:{x=534; y=s.g_origin.y-50} ~surface:surface res;
  draw_window_title s "Rquadtree: union";;

let rquadtree_vertical_symmetry s () =
  let qt = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  ) in
  draw_rquadtree ~scale:s.scale ~g_origin:s.g_origin qt;
  let res = vertical_symmetry qt in
    let g_origin2 = {x=s.g_origin.x+s.base_length+100; y=s.g_origin.y} in
      draw_rquadtree ~scale:s.scale ~g_origin:g_origin2 res;
  draw_window_title s "Rquadtree: vertical symmetry";;

let rquadtree_horizontal_symmetry s () =
  let qt = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  ) and surface = {top=256; right=256; bottom=0; left=0} in
  let g_origin2 = {x=534; y=s.g_origin.y+300} in
    draw_rquadtree ~scale:s.scale ~g_origin:g_origin2 ~surface:surface qt;
  let res = horizontal_symmetry qt in
    let g_origin3 = {x=534; y=s.g_origin.y-50} in
      draw_rquadtree ~scale:s.scale ~g_origin:g_origin3 ~surface:surface res;
  draw_window_title s "Rquadtree: horizontal symmetry";;

let rcquadtree s () =
  let pqt = rcinsert_list ~surface:s.base_surface [
    {top=460; bottom=310; left=230; right=310};
    {top=265; bottom=230; left=300; right=480};
    {top=215; bottom=150; left=110; right=235};
    {top=490; bottom=460; left=410; right=485};
    {top=275; bottom=240; left=260; right=310};
    {top=470; bottom=460; left=2;   right=20};
    {top=250; bottom=210; left=270; right=305};
  ] in
    draw_rcquadtree ~scale:s.scale ~g_origin:{x=406; y=s.g_origin.y} pqt;
  draw_window_title s "RCquadtree";;


let demo_list = [
  (pquadtree_1 g_state);
  (pquadtree_2 g_state);
  (pquadtree_3 g_state);
  (pquadtree_4 g_state);
  (pquadtree_5 g_state);
  (rquadtree_invert g_state);
  (rquadtree_intersection g_state);
  (rquadtree_union g_state);
  (rquadtree_vertical_symmetry g_state);
  (rquadtree_horizontal_symmetry g_state);
  (rcquadtree g_state);
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

let f_init s () =
  let screen_size = Printf.sprintf " %dx%d"
    ((s.base_length*2+s.margin*2+100)*s.scale)
    ((s.base_length+s.margin*2)*s.scale) in

  open_graph screen_size;
  set_line_width s.scale;
  (List.nth demo_list s.pos) ();;

let f_end state () =
  print_string "end";;

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

let start_terminal_demo =
  (*--------------------------------------------------------------------------*)
  print_string "******** Pquadtree ********\n";;

  (*--------------------------------------------------------------------------*)
  print_string "____ pbelong ____\n";;
  let qt1 = pinsert_list [
    {x=40; y=24};
    {x=10; y=10};
  ];;
  let p1 = {x=10; y=10};;
  let res1 = pbelong p1 qt1;;
  Printf.printf "pquadtree:%s\n\npoint:%s\n\nres:%B\n\n"
    (string_of_pquadtree qt1)
    (string_of_point p1)
    res1;;

  (*--------------------------------------------------------------------------*)
  print_string "____ ppath ____\n";;
  let qt2 = pinsert_list [
    {x=40; y=24};
    {x=10; y=10};
    {x=400; y=67};
    {x=5; y=33};
  ];;
  let p2 = {x=5; y=33};;
  let res2 = ppath p2 qt2;;
  Printf.printf "pquadtree:%s\n\npoint:%s\n\nres:%s\n\n"
    (string_of_pquadtree qt2)
    (string_of_point p2)
    (string_of_intercard_list res2);;

  (*--------------------------------------------------------------------------*)
  print_string "____ pinsert ____\n";;
  let qt3 = (pinsert (pinsert PEmpty {x=30; y=30}) {x=300; y=10});;
  Printf.printf "res:%s\n\n" (string_of_pquadtree qt3);;




  (*--------------------------------------------------------------------------*)
  print_string "******** Rquadtree ********\n";;

  (*--------------------------------------------------------------------------*)
  print_string "____ invert ____\n";;
  let qt4 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );;
  let res4 = invert qt4;;
  Printf.printf "rquadtree:%s\n\nres:%s\n\n"
    (string_of_rquadtree qt4)
    (string_of_rquadtree res4);;

  (*--------------------------------------------------------------------------*)
  print_string "____ intersection ____\n";;
  let qt5_1 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );;
  let qt5_2 = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain White),
    Plain White
  );;
  let res5 = intersection qt5_1 qt5_2;;
  Printf.printf "rquadtree1:%s\n\nrquadtree2:%s\n\nres:%s\n\n"
    (string_of_rquadtree qt5_1)
    (string_of_rquadtree qt5_2)
    (string_of_rquadtree res5);;

  (*--------------------------------------------------------------------------*)
  print_string "____ union ____\n";;
  let qt6_1 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );;
  let qt6_2 = RQ (
    Plain Black,
    Plain Black,
    RQ (Plain Black, Plain White, Plain White, Plain White),
    Plain White
  );;
  let res6 = intersection qt6_1 qt6_2;;
  Printf.printf "rquadtree1:%s\n\nrquadtree2:%s\n\nres:%s\n\n"
    (string_of_rquadtree qt6_1)
    (string_of_rquadtree qt6_2)
    (string_of_rquadtree res6);;


  (*--------------------------------------------------------------------------*)
  print_string "____ vertical_symmetry ____\n";;
  let qt7 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );;

  let res7 = vertical_symmetry qt7;;
  Printf.printf "rquadtree:%s\n\nres:%s\n\n"
    (string_of_rquadtree qt7)
    (string_of_rquadtree res7);;

  (*--------------------------------------------------------------------------*)
  print_string "____ horizontal_symmetry ____\n";;
  let qt8 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );;

  let res8 = horizontal_symmetry qt8;;
  Printf.printf "rquadtree:%s\n\nres:%s\n\n"
    (string_of_rquadtree qt8)
    (string_of_rquadtree res8);;

  (*--------------------------------------------------------------------------*)
  print_string "____ encode ____\n";;
  let qt9 = RQ (
    Plain Black,
    Plain White,
    RQ (Plain Black, Plain White, Plain White, Plain Black),
    Plain White
  );;
  let res9 = encode qt9;;
  Printf.printf "rquadtree:%s\n\nres:%s\n\n"
    (string_of_rquadtree qt9)
    (string_of_encoding res9);;

  (*--------------------------------------------------------------------------*)
  print_string "____ decode ____\n";;
  let qt10 = [0;1;1;1;0;0;1;1;1;0;1;0;1;1;1;0];;
  let res10 = decode qt10;;
  Printf.printf "rquadtree:%s\n\nres:%s\n\n"
    (string_of_encoding qt10)
    (string_of_rquadtree res10);;




  (*--------------------------------------------------------------------------*)
  print_string "******** RCquadtree ********\n";;

  (*--------------------------------------------------------------------------*)
  print_string "____ rcinsert ____\n";;
  let qt11 = rcinsert_list [
    {top=100; bottom=10; left=100; right=300};
    {top=100; bottom=10; left=100; right=200};
  ];;
  Printf.printf "rcquadtree:%s\n\n"
   (string_of_rcquadtree qt11);;

  (*--------------------------------------------------------------------------*)
  print_string "____ rccontain ____\n";;
  let qt12 = rcinsert_list [
    {top=460; bottom=310; left=230; right=310};
    {top=265; bottom=230; left=300; right=480};
    {top=215; bottom=150; left=110; right=235};
    {top=490; bottom=460; left=410; right=485};
    {top=275; bottom=240; left=260; right=310};
    {top=470; bottom=460; left=2;   right=20};
    {top=250; bottom=210; left=270; right=305};
  ];;
  let p12 = {x=303; y=245};;
  let res12 = rccontain qt12 p12;;
  Printf.printf "rcquadtree:%s\n\npoint:%s\n\nres:%s\n\n"
    (string_of_rcquadtree qt12)
    (string_of_point p12)
    (string_of_rectangle_list res12);;

let start_graphic_demo () =
  skel (f_init g_state) (f_end g_state) (f_key g_state)
    (f_except g_state);;


let () =
  (****************************************************************************)
  (*                             Graphical demo                               *)
  (****************************************************************************)
  start_graphic_demo ();;

  (****************************************************************************)
  (*                             Terminal demo                                *)
  (****************************************************************************)
  (* start_terminal_demo;; *)
