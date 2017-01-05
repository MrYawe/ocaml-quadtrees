(**
  The type pquadtree and its operations are designed to represent a
  point quadtree.

  A point quadtree is an adaptation of a binary tree used to represent
  two-dimensional point data.

  @author Yannis Weishaupt
 *)

open Point;;
open Rectangle;;

(**
  The sum type representing the point quadtree with:
  {ul {- [PEmpty] the empty pquadtree}
      {- [PNode] a node of the pquadtree}}

  Each [PNode] contains:
  {ul {- its surface represented by a {!type: Rectangle.rect}}
      {- its point represented by a {!type: Point.point}}
      {- 4 children {!type:Pquadtree.pquadtree}}}
 *)
type pquadtree =
    PEmpty
  | PNode of point*rect*pquadtree*pquadtree*pquadtree*pquadtree;;

(**
  Exception raised when a given pquadtree is inconsistent.

  See {!val:Pquadtree.is_consistent}.
 *)
exception InconsistentPquadtree;;

(**
  Exception raised when a given [PNode] is inconsistent.
 *)
exception InconsistentPNode;;

(**
  Exception raised when a point is out of range of a pquadtree.
 *)
exception PointOutOfRange;;

(**
  Exception raised when a surface is invalid.

  An valid surface is a rectangle where its width and its height are
  a power of two.
 *)
exception InvalidSurface;;

(**
  The default width and height of the surface of a pquadtree.

  Its value is [512].
 *)
let base_length = 512;;

(**
  The default default surface of a pquadtree.

  Its value is [{top=base_length; right=base_length; bottom=0; left=0}].
 *)
let base_surface = {top=base_length; right=base_length; bottom=0; left=0};;

(**
  Return [true] if the given pquadtree is consistent and [false] otherwise.

  A consistent pquadtree is a pquadtree where on each node:
  {ul {- the surface is a rectangle where the width and the height is
          a power of two }
      {- the point is contained in the surface}
      {- each child pquadtree is consistent}}
 *)
let rec is_consistent = function
  | PEmpty -> true
  | PNode (p, r, q1, q2, q3, q4) ->
    (rect_is_a_power_of_two r) && (contain_point p r) &&
    (is_consistent q1) &&
    (is_consistent q2) &&
    (is_consistent q3) &&
    (is_consistent q4);;

(**
  Return the child pquadtree corresponding to the given intercardinal directions
  of a {!type:Pquadtree.pquadtree.PNode}.

  Raise [InconsistentPNode] if the given pquadtree is not a
  {!type:Pquadtree.pquadtree.PNode} or if the given [PNode] is inconsistent.
 *)
let get_pquadtree_at_pole pole pqt = match pqt, pole with
  | PNode (_, _, q1, _, _, _), NW -> q1
  | PNode (_, _, _, q2, _, _), NE -> q2
  | PNode (_, _, _, _, q3, _), SW -> q3
  | PNode (_, _, _, _, _, q4), SE -> q4
  | _ -> raise InconsistentPNode;;

(**
  Return [true] if the given point is contained in the given pquadtree and
  [false] otherwise.
 *)
let rec pbelong point pqt =
  if not (is_consistent pqt) then raise InconsistentPquadtree;
  match pqt with
  | PEmpty -> false
  | PNode (p, _, _, _, _, _) when p=point -> true
  | PNode (p, r, q1, q2, q3, q4) ->
    let pole = get_pole point r in
      let pqt = get_pquadtree_at_pole pole (PNode (p, r, q1, q2, q3, q4)) in
        pbelong point pqt;;

(**
  Return the path to reach the given point in the given pquadtree.

  Raise [Failure] if the point is not found.
 *)
let ppath point pqt =
  if not (is_consistent pqt) then raise InconsistentPquadtree;
  let rec aux acc point = function
  | PEmpty -> failwith "no path found"
  | PNode (p, _, _, _, _, _) when p=point -> acc
  | PNode (p, r, q1, q2, q3, q4) ->
    let pole = get_pole point r in
      aux (pole::acc) point
        (get_pquadtree_at_pole pole(PNode (p, r, q1, q2, q3, q4)))
  in List.rev (aux [] point pqt);;

(**
  Insert the given point in the given pquadtree and return the new pquadtree.

  Raise [InvalidSurface] if the surface is invalid. See
  {!exception:Pquadtree.InvalidSurface}.

  Raise [PointOutOfRange] if the given point if out of range of the given
  pquadtree.

  Raise [InconsistentPquadtree] if the given pquadtree is inconsistent. See
  {!val:Pquadtree.is_consistent}.

  @param surface Optional parameter representing the surface of the first
  [PNode] of a pquadtree. This parameter will be used if the given pquadtree
  is [PEmpty]. Default is {!val:Pquadtree.base_surface}.
 *)
let rec pinsert ?(surface = base_surface) pqt point =
  if not (rect_is_a_power_of_two surface) then raise InvalidSurface;
  if not (contain_point point surface) then raise PointOutOfRange;
  if not (is_consistent pqt) then raise InconsistentPquadtree;
  match pqt with
  | PEmpty -> PNode (point, surface, PEmpty, PEmpty, PEmpty, PEmpty)
  | PNode (p, r, q1, q2, q3, q4) ->
    let pole = get_pole point r in
    let new_rect = get_rect_at_pole pole r in (match pole with
      | NW -> PNode (p, r, (pinsert ~surface:new_rect q1 point), q2, q3, q4)
      | NE -> PNode (p, r, q1, (pinsert ~surface:new_rect q2 point), q3, q4)
      | SW -> PNode (p, r, q1, q2, (pinsert ~surface:new_rect q3 point), q4)
      | SE -> PNode (p, r, q1, q2, q3, (pinsert ~surface:new_rect q4 point)));;

(**
  Insert all given points in a new pquadtree and return
  the new pquadtree.

  Raise [InvalidSurface] if the surface is invalid. See
  {!exception:Pquadtree.InvalidSurface}.

  Raise [PointOutOfRange] if the given point if out of range of the given
  pquadtree.

  Raise [InconsistentPquadtree] if the given pquadtree is inconsistent. See
  {!val:Pquadtree.is_consistent}.

  @param surface Optional parameter representing the surface of the first
  [PNode] of a pquadtree. This parameter will be used if the given pquadtree
  is [PEmpty]. Default is {!val:Pquadtree.base_surface}.
 *)
let rec pinsert_list ?(surface = base_surface) li =
  List.fold_left (pinsert ~surface: surface) PEmpty li;;

(**
  Return the string representation of the given pquadtree.

  @param indent Optional parameter representing the number of spaces of
  the indentation. Default is [0].
 *)
let rec string_of_pquadtree ?(indent=0) pqt =
  if not (is_consistent pqt) then raise InconsistentPquadtree;
  match pqt with
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

(**
  The default graphical origin used by draw functions of this module.

  Its value is [{x=0; y=0}].
 *)
let base_g_origin = {x=0; y=0};;

(**
  Draw the given pquadtree with the graphic module of OCaml.

  Raise [InconsistentPquadtree] if the given pquadtree is inconsistent.

  @param scale Optional scaling parameter. For example if [scale = 2] a
  pquadtree's surface of height [10] will be drawn with a height
  of [20] pixels. Default is [1].
  @param g_origin Optional parameter representing the graphical origin of the
  coordinate system where the pquadtree is drawn. Default is
  {!val:Pquadtree.base_g_origin}.
 *)
let rec draw_pquadtree ?(scale=1) ?(g_origin = base_g_origin) pqt =
  if not (is_consistent pqt) then raise InconsistentPquadtree;
  match pqt with
  | PEmpty -> ()
  | PNode (p, r, q1, q2, q3, q4) ->
    draw_point ~scale:scale ~g_origin:g_origin p;
    draw_rectangle ~scale:scale ~g_origin:g_origin r;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q1;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q2;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q3;
    draw_pquadtree ~scale:scale ~g_origin:g_origin q4;;
