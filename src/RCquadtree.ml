open Rectangle;;

type rcquadtree =
    RCEmpty
  | RCNode of rect * (rect list) * (rect list) * rcquadtree * rcquadtree * rcquadtree * rcquadtree;;

let rec draw_rcquadtree scale = function
  | RCEmpty -> ()
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    draw_rectangle scale s;
    draw_median scale s;
    Graphics.set_color Graphics.blue;
    List.iter (draw_rectangle scale) lv;
    List.iter (draw_rectangle scale) lh;
    Graphics.set_color Graphics.black;
    draw_rcquadtree scale q1;
    draw_rcquadtree scale q2;
    draw_rcquadtree scale q3;
    draw_rcquadtree scale q4;;
