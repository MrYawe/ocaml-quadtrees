type point = {x: int; y: int};;
exception InconsistentPoint;;
exception MedianCrossed;;

let point_equal p1 p2 =
  match (p1, p2) with
  | (p1, p2) when p1.x=p2.x && p1.y=p2.y -> true
  | _ -> false;;

let draw_point ?(scale=1) point =
  let size = 3 in
    Graphics.moveto ((point.x-size)*scale) (point.y*scale);
    Graphics.lineto ((point.x+size)*scale) (point.y*scale);

    Graphics.moveto (point.x*scale) ((point.y-size)*scale);
    Graphics.lineto (point.x*scale) ((point.y+size)*scale);;

let string_of_point point =
  Printf.sprintf "{x=%d; y=%d}" point.x point.y;;
