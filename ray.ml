(*
let lambert_up () = let
    theta = asin (sqrt (rnd())) and
    phi   = 2.0 *. pi *. (rnd()) in
    {x = sin theta *. cos phi; y = sin theta *. sin phi; z = cos theta}
*)

open Printf;;

let pi = acos (-1.0)
let () = Dsfmt.init_int 15
let rnd = Dsfmt.genrand
(*
let leafrad = 0.050 (* leaf diameter 100 mm *)
*)
let leafrad = 1. (* leaf diameter 100 mm *)

type vec_t  = {x:float; y:float; z:float} (* 3d vector *)
type leaf_t = {lr:vec_t; ld:vec_t}
              (* lr: location, ld: direction of surface normal *)
type ray_t  = {rr:vec_t; rd:vec_t} (* rr: location, rd: direction *)

let ( *| ) k a = {x =   k *. a.x; y =   k *. a.y; z =   k *. a.z}
let ( +| ) a b = {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z}
let ( -| ) a b = {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z}
let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z

let sph_up () = (* spherically distributed vec with z >= 0 *)
  let theta = acos(rnd())
  and phi = 2. *. pi *. (rnd()) in
  {x = sin theta *. cos phi; y = sin theta *. sin phi; z = cos theta}
      
let r_leaf () =
  let r = {x = 0.; y = 0.; z = 0.}
  and d = sph_up() in  
  {lr = r; ld = d}

let r_ray () =
  let yy = 2. *. rnd() -. 1.
  and zz = 2. *. rnd() -. 1. in
  let r = {x =  2.; y = yy; z = zz}
  and d = {x = -1.; y = 0.; z = 0.} in  
  {rr = r; rd = d}

let close_p a b r =
  let x = a.x -. b.x
  and y = a.y -. b.y
  and z = a.z -. b.z in
  x*.x +. y*.y +. z*.z < r*.r

type hit_t = Miss | Hit of vec_t

let hit_p ray leaf =
  let denom = dot ray.rd leaf.ld in 
    if denom = 0. then Miss
    else
      let numer = dot (leaf.lr -| ray.rr) leaf.ld in
      let dist = numer /. denom in
	if dist <= 0. then Miss
	else let hitpoint = ray.rr +| (dist *| ray.rd) in 
	  if close_p leaf.lr hitpoint leafrad
	  then Hit(hitpoint)
	  else Miss

let nn = 100000;;

let cnt = ref 1 in
  for i = 1 to nn do
    cnt := !cnt +
    if (hit_p (r_ray()) (r_leaf()) <> Miss)
    then 1
    else 0
  done;
  printf "%15.10f\n" (float !cnt /. float nn);
  printf "%15.10f\n" (pi /. 8.)
;;
