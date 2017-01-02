open Rectangle;;

type rcquadtree =
    RCEmpty
  | RCNode of rect * (rect list) * (rect list) *
      rcquadtree * rcquadtree * rcquadtree * rcquadtree;;

exception InconsistentRCquadtree;;
exception InconsistentRCNode;;

let base_length = 512;;
let base_surface = {top=base_length; right=base_length; bottom=0; left=0};;

let rec draw_rcquadtree ?(scale=1)  = function
  | RCEmpty -> ()
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    draw_rectangle ~scale:scale s;
    draw_median ~scale:scale s;
    Graphics.set_color Graphics.blue;
    List.iter (draw_rectangle ~scale:scale) lv;
    List.iter (draw_rectangle ~scale:scale) lh;
    Graphics.set_color Graphics.black;
    draw_rcquadtree ~scale:scale q1;
    draw_rcquadtree ~scale:scale q2;
    draw_rcquadtree ~scale:scale q3;
    draw_rcquadtree ~scale:scale q4;;

let rec print_rcquadtree = function
  | RCEmpty -> ()
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    Printf.printf "s: top=%d; right=%d; bottom=%d; left=%d;\n" s.top s.right s.bottom s.left;
    Printf.printf "**q1**"; print_rcquadtree q1; Printf.printf "\n";
    Printf.printf "**q2**"; print_rcquadtree q2; Printf.printf "\n";
    Printf.printf "**q3**"; print_rcquadtree q3; Printf.printf "\n";
    Printf.printf "**q4**"; print_rcquadtree q4; Printf.printf "\n";;

let rec rcinsert ?(surface = base_surface) rect rcquadtree =
  let aux = function
    | RCNode (s, lv, lh, q1, q2, q3, q4) ->
      let c = center s in (
        match (get_pole_rect rect s) with
        | NO -> RCNode (s, lv, lh, (rcinsert ~surface:{top=s.top; right=c.x; bottom=c.y; left=s.left} rect q1), q2, q3, q4)
        | NE -> RCNode (s, lv, lh, q1, (rcinsert ~surface:{top=s.top; right=s.right; bottom=c.y; left=c.x} rect q2), q3, q4)
        | SO -> RCNode (s, lv, lh, q1, q2, (rcinsert ~surface:{top=c.y; right=c.x; bottom=s.bottom; left=s.left} rect q3), q4)
        | SE -> RCNode (s, lv, lh, q1, q2, q3, (rcinsert ~surface:{top=c.y; right=s.right; bottom=s.bottom; left=c.x} rect q4))
      )
    | _ -> raise InconsistentRCNode
  in match rcquadtree with
  | RCEmpty when vertical_median_cross surface rect ->
    RCNode (surface, [rect], [], RCEmpty, RCEmpty, RCEmpty, RCEmpty)
  | RCEmpty when horizontal_median_cross surface rect ->
    RCNode (surface, [], [rect], RCEmpty, RCEmpty, RCEmpty, RCEmpty)
  | RCEmpty ->
    aux (RCNode (surface, [], [], RCEmpty, RCEmpty, RCEmpty, RCEmpty))
  | RCNode (s, lv, lh, q1, q2, q3, q4) when vertical_median_cross s rect ->
    RCNode (s, rect::lv, lh, q1, q2, q3, q4)
  | RCNode (s, lv, lh, q1, q2, q3, q4) when horizontal_median_cross s rect ->
    RCNode (s, lv, rect::lh, q1, q2, q3, q4)
  | RCNode (s, lv, lh, q1, q2, q3, q4) ->
    aux (RCNode (s, lv, lh, q1, q2, q3, q4));;

(* TODO: fold_left because the order is inversed *)
let rcinsert_list ?(surface = base_surface) rect_list =
  List.fold_right (rcinsert ~surface: surface) rect_list RCEmpty;;

let rccontain rcquadtree p =
  let rec aux acc = function
    | RCEmpty -> acc
    | RCNode (s, lv, lh, q1, q2, q3, q4) ->
      let res = contain_in_list p (lv@lh) in (
        match (get_pole p s) with
        | NO -> aux (res@acc) q1
        | NE -> aux (res@acc) q2
        | SO -> aux (res@acc) q3
        | SE -> aux (res@acc) q4
      )
  in aux [] rcquadtree;;
