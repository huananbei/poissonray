open Printf;;

let pi = acos (-1.0)
let () = Dsfmt.init_int 15
let rnd () = Dsfmt.genrand ()
let leafrad = 0.50 (* leaf radius 100 mm *)

type vec  = {x:float; y:float; z:float} (* 3d vector *)
type leaf = {r:vec; d:vec} (* r: location, d: direction of surface normal *)
let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z

let lambert_up () = let
    theta = asin (sqrt (rnd())) and
    phi   = 2.0 *. pi *. (rnd()) in
    {x = sin theta *. cos phi; y = sin theta *. sin phi; z = cos theta}

;;

let a = (lambert_up()) in dot a a
