open Point;;
open Rectangle;;

type pquadtree =
    PEmpty
  | PNode of point*rect*pquadtree*pquadtree*pquadtree*pquadtree;;

exception InconsistentPquadtree;;
exception InconsistentPNode;;
exception NoPathFound;;

let base_length = 512;;
let base_surface = {top=base_length; right=base_length; bottom=0; left=0};;
let base_g_origin = {x=0; y=0};;

(*
  On part du principe que si il se trouve sur la médiane verticale,
  alors il appartient au quadrant de droite,
  médiane horizontale il appartient au quadrant du dessus
*)

let get_pquadtree_at_pole pole pquadtree = match pquadtree, pole with
  | PNode (_, _, q1, _, _, _), NO -> q1
  | PNode (_, _, _, q2, _, _), NE -> q2
  | PNode (_, _, _, _, q3, _), SO -> q3
  | PNode (_, _, _, _, _, q4), SE -> q4
  | PEmpty, _ -> raise InconsistentPNode;;

let rec pbelong point = function
  | PEmpty -> false
  | PNode (p, _, _, _, _, _) when p=point -> true
  | PNode (p, r, q1, q2, q3, q4) ->
    let pole = get_pole point r in
      let pqt = get_pquadtree_at_pole pole (PNode (p, r, q1, q2, q3, q4)) in
        pbelong point pqt;;

let ppath point pquadtree =
  let rec aux acc point = function
  | PEmpty -> raise NoPathFound
  | PNode (p, _, _, _, _, _) when p=point -> []
  | PNode (p, r, q1, q2, q3, q4) ->
    let pole = get_pole point r in
      aux (pole::acc) point
        (get_pquadtree_at_pole pole(PNode (p, r, q1, q2, q3, q4)))
  in List.rev (aux [] point pquadtree);;

let rec pinsert ?(surface = base_surface) pquadtree point = match pquadtree with
  | PEmpty -> PNode (point, surface, PEmpty, PEmpty, PEmpty, PEmpty)
  | PNode (p, r, q1, q2, q3, q4) ->
    let pole = get_pole point r in
    let new_rect = get_rect_at_pole pole r in (match pole with
      | NO -> PNode (p, r, (pinsert ~surface:new_rect q1 point), q2, q3, q4)
      | NE -> PNode (p, r, q1, (pinsert ~surface:new_rect q2 point), q3, q4)
      | SO -> PNode (p, r, q1, q2, (pinsert ~surface:new_rect q3 point), q4)
      | SE -> PNode (p, r, q1, q2, q3, (pinsert ~surface:new_rect q4 point)));;

let rec pinsert_list ?(surface = base_surface) li =
  List.fold_left (pinsert ~surface: surface) PEmpty li;;

let rec draw_pquadtree ?(scale=1) ?(g_origin = base_g_origin) = function
  | PEmpty -> ()
  | PNode (p, r, q1, q2, q3, q4) ->
    draw_point ~scale:scale ~g_origin:g_origin p;
    draw_rectangle ~scale:scale ~g_origin:g_origin r;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q1;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q2;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q3;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q4;;

let rec string_of_pquadtree ?(indent=0) = function
  | PEmpty -> "PEmpty"
  | PNode (p, r, q1, q2, q3, q4) ->
    let is = String.make indent ' ' and
    ps = string_of_point p and
    rs = string_of_rectangle r and
    q1s = string_of_pquadtree ~indent:(indent+3) q1 and
    q2s = string_of_pquadtree ~indent:(indent+3) q2 and
    q3s = string_of_pquadtree ~indent:(indent+3) q3 and
    q4s = string_of_pquadtree ~indent:(indent+3) q4 in
      Printf.sprintf "\n%sp:%s\n%sr:%s\n%sq1:%s\n%sq2:%s\n%sq3:%s\n%sq4:%s"
        is ps is rs is q1s is q2s is q3s is q4s;;
