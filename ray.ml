(*
let lambert_up () = let
    theta = asin (sqrt (rnd())) and
    phi   = 2.0 *. pi *. (rnd()) in
    {x = sin theta *. cos phi; y = sin theta *. sin phi; z = cos theta}
*)

open Printf;;

let seed = 7

let () = Dsfmt.init_int seed
let rnd = Dsfmt.genrand
let gsl_seed = Nativeint.of_int seed
let gslrng = Gsl_rng.make Gsl_rng.MT19937
let () = Gsl_rng.set gslrng gsl_seed
let poisson lambda = Gsl_randist.poisson gslrng lambda

let lai        = 1.
let leafrad    = 0.050 (* 0.05 leaf radius 50 mm, diameter 100 mm *)
let dimx, dimy, dimz = 3., 3., 1.

let volume = dimx *. dimy *. dimz
let ground_area = dimx *. dimy
let pi = acos (-1.0)
let area_of_leaf = pi *. leafrad *. leafrad
let expected_n_leaves = ground_area *. lai /. area_of_leaf

let n_leaves = poisson expected_n_leaves
let () =  printf "n_leaves: %n\n" n_leaves

type vec_t  = {x:float; y:float; z:float} (* 3d vector *)
type leaf_t = {lr:vec_t; ld:vec_t}        (* lr: location, ld: direction *)
type ray_t  = {rr:vec_t; rd:vec_t}        (* rr: location, rd: direction *)

let vec0 = {x = 0.; y = 0.; z = 0.}
let leaf0 = {lr = vec0; ld = vec0}

let ( + ) = Pervasives.( +. )
let ( - ) = Pervasives.( -. )
let ( * ) = Pervasives.( *. )
let ( / ) = Pervasives.( /. )
let ( ~- ) = Pervasives.( ~-. ) (* Unary negation *)
let ( +. ) = Pervasives.( + )
let ( -. ) = Pervasives.( - )

(* vector operations *)
let ( *| ) k a = {x =   k * a.x; y =   k * a.y; z =   k * a.z}
let ( +| ) a b = {x = a.x + b.x; y = a.y + b.y; z = a.z + b.z}
let ( -| ) a b = {x = a.x - b.x; y = a.y - b.y; z = a.z - b.z}
let dot a b = a.x * b.x + a.y * b.y + a.z * b.z

(* spherically orientated vec with z >= 0 *)
let sph_up () = 
  let theta = acos(rnd()) and 
      phi = 2. * pi * (rnd()) in 
  {x = sin theta * cos phi; y = sin theta * sin phi; z = cos theta}

(* spherically orientated vec with z <= 0 *)
let sph_down () = 
  let theta = acos(rnd()) and 
      phi = 2. * pi * (rnd()) in 
  {x = sin theta * cos phi; y = sin theta * sin phi; z = - cos theta}

(* spherically orientated leaf in origo *)
let r_leaf0 () =
  let r = {x = 0.; y = 0.; z = 0.} and 
      d = sph_up() in  
  {lr = r; ld = d}

(* spherically orientated leaf in a box *)
let r_leaf dx dy dz =
  let r = {x = dx * rnd(); y = dy * rnd(); z = dz * rnd()} in
  {lr = r; ld = sph_up()}

let close_p a b r =
  let dx = a.x - b.x and
      dy = a.y - b.y and
      dz = a.z - b.z in
  dx*dx + dy*dy + dz*dz < r*r

type hit_t = Miss | Hit of vec_t * float

let hit_leaf ray leaf =
  let denom = dot ray.rd leaf.ld in 
  if denom = 0. then Miss
  else
    let numer = dot (leaf.lr -| ray.rr) leaf.ld in
    let distance = numer / denom in
    if distance <= 0. then Miss
    else let hitpoint = ray.rr +| (distance *| ray.rd) in 
	 if close_p hitpoint leaf.lr leafrad
	 then Hit(hitpoint, distance)
	 else Miss

let hit_list ray list =
  let closer_hit hit leaf =
    match hit, (hit_leaf ray leaf) with
      | Miss, Miss  ->  Miss
      | h1  , Miss  ->  h1
      | Miss, h2    ->  h2
      | Hit(v1,d1), Hit(v2,d2) when d1 <= d2  ->  Hit(v1,d1)
      | Hit(v1,d1), Hit(v2,d2)                ->  Hit(v2,d2) in
  Array.fold_left closer_hit Miss list


let test_ray1 () =
  let yy = (2. * rnd() - 1.) * leafrad and 
      zz = (2. * rnd() - 1.) * leafrad in
  let r = {x =  2.; y = yy; z = zz} and
      d = {x = -1.; y = 0.; z = 0.} in  
  {rr = r; rd = d}
    
(* shoot rays from a square to one randomly oriented leaf in origo *)
let test1 n =
  let cnt = ref 1 in
    for i = 1 to n do
      cnt := !cnt +.
	if (hit_leaf (test_ray1()) (r_leaf0()) <> Miss)
	then 1
	else 0
    done ;
    printf "res:  %15.10f\n" (float !cnt /. float n);
    printf "pi/8: %15.10f\n" (pi /. 8.)

let mk_boxforest dx dy dz n =
  let forest = Array.make n leaf0 in
  for i = 0 to n -. 1 do
    Array.set forest i (r_leaf dx dy dz)
  done ;
  forest

let boxtop_ray dx dy dz =
  let frac = 1./3. in
  let xx = frac * (dx + dx * rnd()) and
      yy = frac * (dy + dy * rnd()) in
  let r = {x = xx; y = yy; z = dz + leafrad} in
  {rr = r; rd = {x = 0.; y = 0.; z = -1.}}

let test_ray2 () = boxtop_ray dimx dimy dimx
let forest1 = mk_boxforest dimx dimy dimz n_leaves 

(* shoot rays from top down to boxforest *)
let test2 n =
  let cnt = ref 1 in
    for i = 1 to n do
      cnt := !cnt +.
	if (hit_list (test_ray2 ()) forest1 <> Miss)
	then 1
	else 0
    done ;
    printf "res:           %15.10f\n" (float !cnt /. float n);
    printf "1 - exp(-1/2): %15.10f\n" (1. - exp (-0.5))

let () = printf "len: %n\n" (Array.length forest1) ;;
let () = test2 10000 ;;

(*
let r1 = {rr = {x = 0.; y = 0.; z = 10.}; rd = {x = 0.; y = 0.; z = -1.}} ;;
let l1 = {lr = {x = 0.; y = 0.; z = 0.}; ld = {x = 0.; y = 0.; z = 1.}} ;;
let f1 = Array.make 1 l1 ;;
hit_list r1 f1 ;;

let () = test1 100000 ;;
*)
