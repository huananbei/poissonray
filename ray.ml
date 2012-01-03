(*
let lambert_up () = let
    theta = asin (sqrt (rnd())) and
    phi   = 2.0 *. pi *. (rnd()) in
    {x = sin theta *. cos phi; y = sin theta *. sin phi; z = cos theta}
*)

open Printf;;
let () = Dsfmt.init_int 15
let rnd = Dsfmt.genrand

let lai        = 3.
let leafrad    = 0.100 (* 0.05 leaf radius 50 mm, diameter 100 mm *)
let dimx, dimz = 3., 3.

let dimy = dimx
let volume = dimx *. dimy *. dimz
let ground_area = dimx *. dimy
let pi = acos (-1.0)
let leaf_area = pi *. leafrad *. leafrad
let expected_n_leaves = ground_area *. lai /. leaf_area

(* http://www.johndcook.com/blog/2010/08/16/how-to-compute-log-factorial/ *)
(* Not good for x < 256 *)
let logfact x =
  (x -. 0.5) *. log(x) -. x +. 0.5 *. log(2. *. pi) +. 1. /. (12. *. x)

(* http://www.johndcook.com/blog/2010/06/14/generating-poisson-random-values/ *)
let poisson lambda =
  let c = 0.767 -. 3.36 /. lambda
  and beta = pi /. sqrt(3. *. lambda) in
  let alpha = beta *. lambda
  and k = log(c) -. lambda -. log(beta) in
  let u = ref 0. and x = ref 0. and n = ref 0 and v = ref 0. and y = ref 0.
  and lhs = ref 1. and rhs = ref 0. in
    while lhs > rhs do
      n := -1;
      while !n < 0 do
	u := rnd();
	while !u = 0. do u := rnd() done;
	x := (alpha -. log((1. -. !u) /. !u)) /. beta;
	n := int_of_float (floor (!x +. 0.5))
      done;
      v := rnd();
      y := alpha -. beta *. !x;
      lhs := !y +. log(!v /. (1. +. exp(!y))**2.);
      rhs := k +. (float !n) *. log(lambda) -. logfact(float !n);
    done;
    !n

let n_leaves = poisson expected_n_leaves

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
	  if close_p hitpoint leaf.lr leafrad
	  then Hit(hitpoint)
	  else Miss

let test1 nn =
  (* shoot rays from a square to one randomly oriented leaf in origo *)
  let r_ray () =
    let yy = 2. *. rnd() *. leafrad -. leafrad
    and zz = 2. *. rnd() *. leafrad -. leafrad  in
    let r = {x =  2.; y = yy; z = zz}
    and d = {x = -1.; y = 0.; z = 0.} in  
      {rr = r; rd = d}
  in
  let cnt = ref 1 in
    for i = 1 to nn do
      cnt := !cnt +
	if (hit_p (r_ray()) (r_leaf()) <> Miss)
	then 1
	else 0
    done;
    printf "res:  %15.10f\n" (float !cnt /. float nn);
    printf "pi/8: %15.10f\n" (pi /. 8.)
;;
