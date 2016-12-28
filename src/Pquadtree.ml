open Rectangle;;

type pquadtree =
    PEmpty
  | PNode of point*rect*pquadtree*pquadtree*pquadtree*pquadtree;;
let base_length = 512;;
let base_rect = {top=base_length; right=base_length; bottom=0; left=0};;

exception InconsistentPquadtree;;
exception NoPathFound;;

(* let pbelong = function
    PNoeud (p, r, _, _, _, _)
      when p.x>=r.left && p.x<=r.right && p.y>=r.bottom && p.y<=r.top -> true
  | _ -> false;; *)

(*
  On part du principe que si il se trouve sur la médiane verticale,
  alors il appartient au quadrant de droite,
  médiane horizontale il appartient au quadrant du dessus
*)

(* let get_pquadtree pole = function
  | PNode (_, _, q1, _, _, _) when pole=NO -> q1
  | PNode (_, _, _, q2, _, _) when pole=NE -> q2
  | PNode (_, _, _, _, q3, _) when pole=SO -> q3
  | PNode (_, _, _, _, _, q4) -> q4
  | PEmpty ;; *)

let rec pquadtree_equal pquadtree1 pquadtree2 =
  match (pquadtree1, pquadtree2) with
    | (PNode (p1, r1, q11, q12, q13, q14), PNode (p2, r2, q21, q22, q23, q24))
      when (point_equal p1 p2) && (rectangle_equal r1 r2) ->
        (pquadtree_equal q11 q21) && (pquadtree_equal q12 q22) &&
        (pquadtree_equal q13 q23) && (pquadtree_equal q14 q24)
    | (PEmpty, PEmpty) -> true
    | _ -> false;;

let rec pbelong point = function
  | PEmpty -> false
  | PNode (p, _, _, _, _, _) when point_equal p point -> true
  | PNode (p, r, q1, _, _, _) when (get_pole point r)=NO -> pbelong point q1
  | PNode (p, r, _, q2, _, _) when (get_pole point r)=NE -> pbelong point q2
  | PNode (p, r, _, _, q3, _) when (get_pole point r)=SO -> pbelong point q3
  | PNode (p, r, _, _, _, q4) when (get_pole point r)=SE -> pbelong point q4
  | _ -> raise InconsistentPquadtree;;

(* let ppath point quadtree =
  let rec ppath_step acc point = function
    | PEmpty -> raise NoPathFound
    | PNode (p, _, _, _, _, _) when p.x = point.x && p.y = point.y -> acc
    | PNode (p, r, q1, _, _, _) when (get_pole point r)=NO -> ppath_step (NO::acc) point q1
    | PNode (p, r, _, q2, _, _) when (get_pole point r)=NE -> ppath_step (NE::acc) point q2
    | PNode (p, r, _, _, q3, _) when (get_pole point r)=SO -> ppath_step (SO::acc) point q3
    | PNode (p, r, _, _, _, q4) when (get_pole point r)=SE -> ppath_step (SE::acc) point q4
    | _ -> raise InconsistentPquadtree
  in ppath_step [] point quadtree;; *)

let rec ppath point = function
  | PEmpty -> raise NoPathFound
  | PNode (p, _, _, _, _, _) when point_equal p point -> []
  | PNode (p, r, q1, _, _, _) when (get_pole point r)=NO -> NO::(ppath point q1)
  | PNode (p, r, _, q2, _, _) when (get_pole point r)=NE -> NE::(ppath point q2)
  | PNode (p, r, _, _, q3, _) when (get_pole point r)=SO -> SO::(ppath point q3)
  | PNode (p, r, _, _, _, q4) when (get_pole point r)=SE -> SE::(ppath point q4)
  | _ -> raise InconsistentPquadtree;;


let insert point pquadtree =
  let rec insert_step point rect = function
    | PEmpty -> PNode (point, rect, PEmpty, PEmpty, PEmpty, PEmpty)
    | PNode (p, r, q1, q2, q3, q4) when (get_pole point r)=NO ->
      let c = center r in
        PNode (p, r, (insert_step point {top=r.top; right=c.x; bottom=c.y; left=r.left} q1), q2, q3, q4)
    | PNode (p, r, q1, q2, q3, q4) when (get_pole point r)=NE ->
      let c = center r in
        PNode (p, r, q1, (insert_step point {top=r.top; right=r.right; bottom=c.y; left=c.x} q2), q3, q4)
    | PNode (p, r, q1, q2, q3, q4) when (get_pole point r)=SO ->
      let c = center r in
        PNode (p, r, q1, q2, (insert_step point {top=c.y; right=c.x; bottom=r.bottom; left=r.left} q3), q4)
    | PNode (p, r, q1, q2, q3, q4) when (get_pole point r)=SE ->
      let c = center r in
        PNode (p, r, q1, q2, q3, (insert_step point {top=c.y; right=r.right; bottom=r.bottom; left=c.x} q4))
    | _ -> raise InconsistentPquadtree
  in insert_step point base_rect pquadtree;;

let rec insert_list pquadtree = function
  | [] -> pquadtree
  | p::l -> insert_list (insert p pquadtree) l;;

let rec draw_pquadtree scale = function
  | PEmpty -> ()
  | PNode (p, r, q1, q2, q3, q4) ->
    draw_point scale p;
    draw_rectangle scale r;
    draw_pquadtree scale q1; draw_pquadtree scale q2; draw_pquadtree scale q3; draw_pquadtree scale q4;;
