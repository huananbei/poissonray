open Printf
open Batteries_uni

let seed       = 7
let lai        = 1.
let leafrad    = 0.050 (* 0.05 leaf radius 50 mm, diameter 100 mm *)
let xdim, ydim, zdim = 3., 3., 1.
let nx, ny, nz = 6, 6, 2 (* number of boxes in each axis *)

let () = Dsfmt.init_int seed
let rnd = Dsfmt.genrand
let gsl_seed = Nativeint.of_int seed
let gslrng = Gsl_rng.make Gsl_rng.MT19937
let () = Gsl_rng.set gslrng gsl_seed
let poisson lambda = Gsl_randist.poisson gslrng lambda

let ( + ) = Pervasives.( +. )
let ( - ) = Pervasives.( -. )
let ( * ) = Pervasives.( *. )
let ( / ) = Pervasives.( /. )
let ( ~- ) = Pervasives.( ~-. ) (* Unary negation *)
let ( +. ) = Pervasives.( + )
let ( -. ) = Pervasives.( - )

let x_start, x_end = leafrad, xdim - leafrad
let y_start, y_end = leafrad, ydim - leafrad
let z_start, z_end = leafrad, zdim - leafrad
let leafbox = x_start, x_end,  y_start, y_end, z_start, z_end  
let volume = (x_end - x_start) * (y_end - y_start) * (z_end - z_start)
let ground_area = (x_end - x_start) * (y_end - y_start)
let pi = acos (-1.0)
let area_of_leaf = pi * leafrad**2.
let expected_n_leaves = ground_area * lai / area_of_leaf

let n_leaves = poisson expected_n_leaves
let () =  printf "n_leaves: %n\n" n_leaves

type vec_t  = {x:float; y:float; z:float} (* 3d vector *)
type leaf_t = {lpos:vec_t; ldir:vec_t}    (* lpos: position, ldir: direction *)
type ray_t  = {rpos:vec_t; rdir:vec_t}    (* rpos: location, rdir: direction *)
type leaflist_t = leaf_t DynArray.t

let vec0  = {x = 0.; y = 0.; z = 0.}
let leaf0 = {lpos = vec0; ldir = vec0}

(* vector operations *)
let ( *^ ) k a = {x =   k * a.x; y =   k * a.y; z =   k * a.z}
let ( +^ ) a b = {x = a.x + b.x; y = a.y + b.y; z = a.z + b.z}
let ( -^ ) a b = {x = a.x - b.x; y = a.y - b.y; z = a.z - b.z}
let dot a b = a.x * b.x + a.y * b.y + a.z * b.z

(* spherically oriented vec with z >= 0 *)
let sph_up () = 
  let theta = acos(rnd()) and 
      phi = 2. * pi * (rnd()) in 
  {x = sin theta * cos phi; y = sin theta * sin phi; z = cos theta}

(* spherically oriented vec with z <= 0 *)
let sph_down () =
  let v = sph_up() in {v with z = -v.z}

(* spherically oriented leaf in origo *)
let r_leaf0 () =
  let p = {x = 0.; y = 0.; z = 0.} and 
      d = sph_up() in  
  {lpos = p; ldir = d}

(* spherically oriented leaf in a box *)
let r_leaf box =
  let x1, x2, y1, y2, z1, z2 = box in
  let p = {x = x1 + rnd()*(x2-x1); y = y1 + rnd()*(y2-y1); z = z1 + rnd()*(z2-z1)} in
  {lpos = p; ldir = sph_up()}

(* for points (vecs) a and b, is |a-b| < r *)
let close_p a b r =
  let dx = a.x - b.x and dy = a.y - b.y and dz = a.z - b.z in
  dx*dx + dy*dy + dz*dz < r*r

type hit_t = Miss | Hit of vec_t * float

let hit_leaf ray leaf =
  let denom = dot ray.rdir leaf.ldir in 
  if denom = 0. then Miss
  else let numer = dot (leaf.lpos -^ ray.rpos) leaf.ldir in
       let distance = numer / denom in
       if distance <= 0. then Miss
       else let hitpoint = ray.rpos +^ (distance *^ ray.rdir) in 
	    if close_p hitpoint leaf.lpos leafrad
	    then Hit(hitpoint, distance)
	    else Miss

let hit_list ray list =
  let closer_hit hit leaf =
    match hit, (hit_leaf ray leaf) with
      | Miss, Miss  ->  Miss
      | h1  , Miss  ->  h1
      | Miss, h2    ->  h2
      | Hit(pos1,dist1), Hit(pos2,dist2) when dist1 <= dist2  ->  Hit(pos1,dist1)
      | Hit(pos1,dist1), Hit(pos2,dist2)                      ->  Hit(pos2,dist2) in
  Array.fold_left closer_hit Miss list

let test_ray1 () =
  let yy = (2. * rnd() - 1.) * leafrad and 
      zz = (2. * rnd() - 1.) * leafrad in
  let r = {x =  2.; y = yy; z = zz} and
      d = {x = -1.; y = 0.; z = 0.} in  
  {rpos = r; rdir = d}
    
(* shoot rays from a square to one randomly oriented leaf in origo *)
let test1 n =
  let cnt = ref 1 in
    for i = 1 to n do
      cnt := !cnt +.
	if hit_leaf (test_ray1()) (r_leaf0()) <> Miss then 1 else 0
    done ;
    printf "res:  %15.10f\n" (float !cnt /. float n);
    printf "pi/8: %15.10f\n" (pi /. 8.)

let make_boxforest leafbox n =
  let forest = Array.make n leaf0 in
  for i = 0 to n -. 1 do
    forest.(i) <- r_leaf leafbox
  done ;
  forest

let boxtop_ray zdim ydim zdim =
  let frac = 1./3. in
  let xx = frac * (xdim + xdim*rnd()) and
      yy = frac * (ydim + ydim*rnd()) in
  let r = {x = xx; y = yy; z = zdim} in
  {rpos = r; rdir = {x = 0.; y = 0.; z = -1.}}

let test_ray2 () = boxtop_ray xdim ydim zdim
let forest1 = make_boxforest leafbox n_leaves 

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

let () = test2 100

let make_boxgrid nx ny nz =
  let make_leaflist x :leaflist_t = DynArray.create () in
  let make_z_array x = Array.init nz make_leaflist in
  let make_y_matrix x = Array.init ny make_z_array in
  Array.init nx make_y_matrix

let boxgrid = make_boxgrid nx ny nz ;;
