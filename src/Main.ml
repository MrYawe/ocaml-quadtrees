open Rectangle;;
open Pquadtree;;
open Graphics;;

let () =
  (* print_int (Foo.add 3 2);;
  print_string "hello :)";; *)
  print_string "*** Begining ***";;
  print_newline();;


  let pqt = insert {x=400; y=300} (insert {x=10; y=5} PEmpty);;

  try
    Graphics.open_graph " 600x600";
    Graphics.set_window_title "Quadtrees";
    Graphics.set_line_width 5;
    draw_quadtree pqt;
    ignore (Graphics.read_key ());
  with
  | Graphic_failure("fatal I/O error") -> print_string "Fermeture de la fenêtre" ; print_newline()
