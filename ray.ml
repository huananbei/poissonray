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

let lai        = 3.
let leafrad    = 0.100 (* 0.05 leaf radius 50 mm, diameter 100 mm *)
let dimx, dimz = 3., 3.

let dimy = dimx
let volume = dimx *. dimy *. dimz
let ground_area = dimx *. dimy
let pi = acos (-1.0)
let leaf_area = pi *. leafrad *. leafrad
let expected_n_leaves = ground_area *. lai /. leaf_area

let n_leaves = poisson expected_n_leaves
let () =  printf "n_leaves: %n\n" n_leaves

type vec_t  = {x:float; y:float; z:float} (* 3d vector *)
type leaf_t = {lr:vec_t; ld:vec_t}
              (* lr: location, ld: direction of surface normal *)
type ray_t  = {rr:vec_t; rd:vec_t} (* rr: location, rd: direction *)

let vec0 = {x = 0.; y = 0.; z = 0.}
let leaf0 = {lr = vec0; ld = vec0}

let ( + ) = Pervasives.( +. )
let ( - ) = Pervasives.( -. )
let ( * ) = Pervasives.( *. )
let ( / ) = Pervasives.( /. )

let ( +. ) = Pervasives.( + )
let ( -. ) = Pervasives.( - )

let ( *| ) k a = {x =   k * a.x; y =   k * a.y; z =   k * a.z}
let ( +| ) a b = {x = a.x + b.x; y = a.y + b.y; z = a.z + b.z}
let ( -| ) a b = {x = a.x - b.x; y = a.y - b.y; z = a.z - b.z}
let dot a b = a.x * b.x + a.y * b.y + a.z * b.z

(* spherically orientated vec with z >= 0 *)
let sph_up () = 
  let theta = acos(rnd()) and 
      phi = 2. * pi * (rnd()) in 
  {x = sin theta * cos phi; y = sin theta * sin phi; z = cos theta}

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

type hit_t = Miss | Hit of vec_t

let hit_p ray leaf =
  let denom = dot ray.rd leaf.ld in 
    if denom = 0. then Miss
    else
      let numer = dot (leaf.lr -| ray.rr) leaf.ld in
      let dist = numer / denom in
	if dist <= 0. then Miss
	else let hitpoint = ray.rr +| (dist *| ray.rd) in 
	  if close_p hitpoint leaf.lr leafrad
	  then Hit(hitpoint)
	  else Miss

let test1 n =
  (* shoot rays from a square to one randomly oriented leaf in origo *)
  let r_ray () =
    let yy = (2. * rnd() - 1.) * leafrad and 
	zz = (2. * rnd() - 1.) * leafrad in
    let r = {x =  2.; y = yy; z = zz} and
	d = {x = -1.; y = 0.; z = 0.} in  
    {rr = r; rd = d} in
  let cnt = ref 1 in
    for i = 1 to n do
      cnt := !cnt +.
	if (hit_p (r_ray()) (r_leaf0()) <> Miss)
	then 1
	else 0
    done;
    printf "res:  %15.10f\n" (float !cnt /. float n);
    printf "pi/8: %15.10f\n" (pi /. 8.)

(* let () = test1 1000000 ;; *)

let mk_forest1 dx dy dz n =
  let forest = Array.make n leaf0 in
  for i = 0 to n -. 1 do
    Array.set forest i (r_leaf dx dy dz)
  done;
  forest

let forest1 = mk_forest1 dimx dimy dimz n_leaves ;;

printf "len: %n\n" (Array.length forest1) ;;


