(*
let lambert_up () = let
    theta = asin (sqrt (rnd())) and
    phi   = 2.0 *. pi *. (rnd()) in
    {x = sin theta *. cos phi; y = sin theta *. sin phi; z = cos theta}
*)

open Printf;;

let dsfmt_seed = 7
let gsl_seed   = 7n (* n for nativeint *)

let () = Dsfmt.init_int dsfmt_seed
let rnd = Dsfmt.genrand

let gslrng = Gsl_rng.make Gsl_rng.MT19937 ;;
Gsl_rng.set gslrng gsl_seed
let poisson = Gsl_randist.poisson gslrng

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
