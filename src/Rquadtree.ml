(**
  The type rquadtree and its operations are designed to represent a
  region quadtree.

  A region quadtree is an adaptation of a binary tree used to represent a
  black and white image.

  @author Yannis Weishaupt
 *)

open Point;;
open Rectangle;;

(**
  The sum type representing the possible colors of a empty rquadtree.
 *)
type colors = White | Black ;;

(**
  The sum type representing the region quadtree with:
  {ul {- [Plain] the empty rquadtree that will be drawn in black or white}
      {- [RQ] a node of the rquadtree}}

  Each [RQ] contains 4 children of {!type:Rquadtree.rquadtree}.
 *)
type rquadtree =
    Plain of colors
  | RQ of rquadtree * rquadtree * rquadtree * rquadtree;;

(**
  Exception raised when a given encoded rquadtree is inconsistent.
 *)
exception InconsistentEncoding;;

(**
  The default width and height of the surface of a rquadtree.

  Its value is [512].
 *)
let base_length = 512;;

(**
  The default default surface of a rquadtree.

  Its value is [{top=base_length; right=base_length; bottom=0; left=0}].
 *)
let base_surface = {top=base_length; right=base_length; bottom=0; left=0};;

(**
  Invert the color of each [Plain] in the given rquadtree.
 *)
let rec invert = function
  | Plain White -> Plain Black
  | Plain Black -> Plain White
  | RQ (q1, q2, q3, q4) -> RQ (invert q1, invert q2, invert q3, invert q4);;

(**
  Return the rquadtree result of the intersection of two given rquadtree.

  If one pixel of the intersection rquadtree is black then this pixel is black
  in the first and the second given rquadtree.
 *)
let rec intersection rquadtree1 rquadtree2 =
  match (rquadtree1, rquadtree2) with
    | (RQ (q11, q12, q13, q14), RQ (q21, q22, q23, q24)) ->
        RQ (intersection q11 q21, intersection q12 q22, intersection q13 q23,
          intersection q14 q24)
    | (Plain Black, RQ (q1, q2, q3, q4)) | (RQ (q1, q2, q3, q4), Plain Black) ->
      RQ ((intersection (Plain Black) q1), (intersection (Plain Black) q3),
        (intersection (Plain Black) q3), (intersection (Plain Black) q4))
    | (Plain Black, Plain Black) -> Plain Black
    | _ -> Plain White;;

(**
  Return the rquadtree result of the union of two given rquadtree.

  If one pixel of the union rquadtree is black then this pixel is black
  in the first or the second given rquadtree.
 *)
let rec union rquadtree1 rquadtree2 =
  match (rquadtree1, rquadtree2) with
    | (RQ (q11, q12, q13, q14), RQ (q21, q22, q23, q24)) ->
      let res = RQ (union q11 q21, union q12 q22, union q13 q23, union q14 q24)
      in (match res with
        | RQ (Plain Black, Plain Black, Plain Black, Plain Black) -> Plain Black
        | _ -> res)
    | (Plain White, RQ (q1, q2, q3, q4)) | (RQ (q1, q2, q3, q4), Plain White) ->
      RQ ((union (Plain White) q1), (union (Plain White) q3),
        (union (Plain White) q3), (union (Plain White) q4))
    | (Plain White, Plain White) -> Plain White
    | _ -> Plain Black;;

(**
  Perform a vertical symmetry on the given rquadtree and return
  the resulting rquadtree.

  The axis of symmetry is the right border of the rquadtree.
 *)
let rec vertical_symmetry = function
  | Plain c -> Plain c
  | RQ (q1, q2, q3, q4) ->
    RQ (vertical_symmetry q2, vertical_symmetry q1,
      vertical_symmetry q4, vertical_symmetry q3);;

(**
  Perform a horizontal symmetry on the given rquadtree and return
  the resulting rquadtree.

  The axis of symmetry is the bottom border of the rquadtree.
 *)
let rec horizontal_symmetry = function
  | Plain c -> Plain c
  | RQ (q1, q2, q3, q4) ->
    RQ (vertical_symmetry q3, vertical_symmetry q4,
      vertical_symmetry q1, vertical_symmetry q2);;

(**
  Return the binary encoding of the given rquadtree as list of [O] and [1].
 *)
let encode rquadtree =
  let rec code_step acc = function
    | Plain White -> 1::0::acc
    | Plain Black -> 1::1::acc
    | RQ (q1, q2, q3, q4) ->
      0::(code_step (code_step (code_step (code_step acc q4) q3) q2) q1)
  in code_step [] rquadtree;;

(**
  Decode the given rquadtree encoding and return the resulting rquadtree.

  Raise [InconsistentEncoding] if the given encoding is inconsistent.
 *)
let decode l =
  let rec aux = function
    | 1::0::l -> Plain White, l
    | 1::1::l -> Plain Black, l
    | 0::l ->
      let q1, l = aux l in
      let q2, l = aux l in
      let q3, l = aux l in
      let q4, l = aux l in
        RQ (q1, q2, q3, q4), l
    | _ -> raise InconsistentEncoding
  in let rqt, _ = aux l in rqt;;

(**
  Return the string representation of the given rquadtree encoding.
 *)
let rec string_of_encoding encoding =
  let m = List.map string_of_int encoding in
    String.concat "" m;;

(**
  Return the string representation of the given rquadtree.

  @param indent Optional parameter representing the number of spaces of
  the indentation. Default is [0].
 *)
let rec string_of_rquadtree ?(indent=0) = function
  | Plain Black -> "Black"
  | Plain White -> "White"
  | RQ (q1, q2, q3, q4) ->
    let is = String.make indent ' ' and
    q1s = string_of_rquadtree ~indent:(indent+3) q1 and
    q2s = string_of_rquadtree ~indent:(indent+3) q2 and
    q3s = string_of_rquadtree ~indent:(indent+3) q3 and
    q4s = string_of_rquadtree ~indent:(indent+3) q4 in
      Printf.sprintf
        "\n%sq1:%s\n%sq2:%s\n%sq3:%s\n%sq4:%s"
        is q1s is q2s is q3s is q4s;;

(**
  The default graphical origin used by draw functions of this module.

  Its value is [{x=0; y=0}].
 *)
let base_g_origin = {x=0; y=0};;

(**
  Draw the given rquadtree with the graphic module of OCaml.

  @param scale Optional scaling parameter. For example if [scale = 2] a
  rquadtree's surface of height [10] will be drawn with a height
  of [20] pixels. Default is [1].

  @param g_origin Optional parameter representing the graphical origin of the
  coordinate system where the rquadtree is drawn. Default is
  {!val:Rquadtree.base_g_origin}.

  @param surface Optional parameter representing the surface of the first
  [RQ] of a pquadtree. Default is {!val:Rquadtree.base_surface}.
 *)
let rec draw_rquadtree ?(scale=1) ?(g_origin = base_g_origin)
  ?(surface = base_surface) = function
    | Plain White ->
      draw_plain_rectangle ~scale:scale surface ~g_origin:g_origin
        Graphics.white;
      draw_rectangle ~scale:scale ~g_origin:g_origin surface
    | Plain Black ->
      draw_plain_rectangle ~scale:scale surface ~g_origin:g_origin
        Graphics.black;
      draw_rectangle ~scale:scale ~g_origin:g_origin surface
    | RQ (q1, q2, q3, q4) ->
      draw_rquadtree ~scale:scale ~g_origin:g_origin
        ~surface:(get_rect_at_pole NW surface) q1;
      draw_rquadtree ~scale:scale ~g_origin:g_origin
        ~surface:(get_rect_at_pole NE surface) q2;
      draw_rquadtree ~scale:scale ~g_origin:g_origin
        ~surface:(get_rect_at_pole SW surface) q3;
      draw_rquadtree ~scale:scale ~g_origin:g_origin
        ~surface:(get_rect_at_pole SE surface) q4;;
