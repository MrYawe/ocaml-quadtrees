type pole = NO | NE | SO | SE;;

type point = {x: int; y: int};;
exception InconsistentPoint;;

type rect = {top: int; bottom: int; left: int; right: int};;

let center rect = {x=(rect.right-rect.left)/2; y=(rect.top-rect.bottom)/2};;

let get_pole point rect =
  let c = center rect in
    if point.x < c.x && point.y >= c.y then NO
    else if point.x >= c.x && point.y >= c.y then NE
    else if point.x < c.x && point.y < c.y then SO
    else if point.x >= c.x && point.y < c.y then SE
    else raise InconsistentPoint;;
