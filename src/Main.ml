open Rectangle;;
open Pquadtree;;
open Graphics;;

type config = {base_length:int; scale:int;};;

let () =
  (* print_int (Foo.add 3 2);;
  print_string "hello :)";; *)
  print_string "*** Begining ***";;
  print_newline();;

  let config = {base_length=512; scale=3};;


  let pqt = (insert {x=300; y=10} (insert {x=30; y=30} PEmpty));;

  let screen_size = Printf.sprintf " %dx%d"
    ((config.base_length+20)*config.scale) ((config.base_length+20)*config.scale);;

  try
    Graphics.open_graph screen_size;
    Graphics.set_window_title "Quadtrees";
    Graphics.set_line_width config.scale;

    draw_quadtree config.scale pqt;
    ignore (Graphics.read_key ());
  with
  | Graphic_failure("fatal I/O error") -> print_string "Fermeture de la fenêtre" ; print_newline()
