open Printf
(* open Batteries_uni *)

let seed             = 2
let lai              = 100.
let leafrad          = 0.05            (* 0.05 leaf radius 5 cm, diameter 10 cm *)
let xdim, ydim, zdim = 1., 1., 2.
let nx, ny, nz       = 1, 1, 2    (* number of boxes in each axis *)

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
let domain_walls = x_start, x_end,  y_start, y_end, z_start, z_end  
let gridsize = nx, ny, nz
let gridsteps = xdim/float_of_int nx, ydim/float_of_int ny, zdim/float_of_int nz
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
type leaflist_t = leaf_t list

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
  let p = {x = x1 + rnd()*(x2-x1);
	   y = y1 + rnd()*(y2-y1);
	   z = z1 + rnd()*(z2-z1)} in
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
      | Hit(pos1,dist1), Hit(pos2,dist2) when dist1 <= dist2 -> Hit(pos1,dist1)
      | Hit(pos1,dist1), Hit(pos2,dist2) -> Hit(pos2,dist2) in
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

let boxtop_ray zdim ydim zdim =
  let frac = 1./3. in
  let xx = frac * (xdim + xdim*rnd()) and
      yy = frac * (ydim + ydim*rnd()) in
  let r = {x = xx; y = yy; z = zdim} in
  {rpos = r; rdir = {x = 0.; y = 0.; z = -1.}}

let test_ray2 () = boxtop_ray xdim ydim zdim

(* shoot rays from top down to boxforest *)
let test2 leaflist n =
  let cnt = ref 1 in
    for i = 1 to n do
      cnt := !cnt +.
	if (hit_list (test_ray2 ()) leaflist <> Miss) then 1 else 0
    done ;
  printf "%n rays\n" n ;
  printf "  res:          %13.10f\n" (float !cnt /. float n) ;
  printf "  1-exp(-lai/2):%13.10f\n" (1. - exp (- lai/2.))

let make_leafarray domain_walls n =
  let forest = Array.make n leaf0 in
  for i = 0 to n -. 1 do
    forest.(i) <- r_leaf domain_walls
  done ;
  forest

(* make 3d array of empty lists *)
let make_empty_boxgrid size =
  let nx, ny, nz = size in
  let make_leaflist x :leaflist_t = [] in
  let make_z_array x = Array.init nz make_leaflist in
  let make_y_matrix x = Array.init ny make_z_array in
  Array.init nx make_y_matrix

let main_box steps leaf =
  let dx, dy, dz = steps in
  let xi = int_of_float (leaf.lpos.x/dx) and
      yi = int_of_float (leaf.lpos.y/dy) and
      zi = int_of_float (leaf.lpos.z/dz) in
  xi, yi, zi

(* helper functions to add indices of neighboring boxes. this is a mess *)
let add_index idx diff v =
  let v2 = Array.copy v in v2.(idx) <- v.(idx) +. diff ; [v; v2]
let add_smaller_x = add_index 0 (-1)
let add_larger_x  = add_index 0 1
let add_smaller_y lst = List.flatten (List.map (add_index 1 (-1)) lst)
let add_larger_y lst  = List.flatten (List.map (add_index 1 1)    lst)
let add_smaller_z lst = List.flatten (List.map (add_index 2 (-1)) lst)
let add_larger_z lst  = List.flatten (List.map (add_index 2 1)    lst)

(* find indices of all boxes leaf is in, or close to. this is also a mess *)
let box_index_list steps leaf =
  let dx, dy, dz = steps in
  let xi, yi, zi = main_box steps leaf in
  let idx = [|xi;yi;zi|] in
  let x_list =
    if leaf.lpos.x < dx*(float_of_int xi) + leafrad
    then add_smaller_x idx
    else if leaf.lpos.x > dx*(float_of_int (xi +. 1)) - leafrad
    then add_larger_x idx
    else [idx] in
  let xy_list =
    if leaf.lpos.y < dx*(float_of_int yi) + leafrad
    then add_smaller_y x_list
    else if leaf.lpos.y > dx*(float_of_int (yi +. 1)) - leafrad
    then add_larger_y x_list
    else x_list in
  let xyz_list =
    if leaf.lpos.z < dx*(float_of_int zi) + leafrad
    then add_smaller_z xy_list
    else if leaf.lpos.z > dx*(float_of_int (zi +. 1)) - leafrad
    then add_larger_z xy_list
    else xy_list in
  xyz_list

(* 3d array of lists, each list contains leaves in or near that gridbox *)
let filled_boxgrid size steps leafarray =
  let boxgrid = make_empty_boxgrid size in
  let put_into_boxes leaf = 
    let index_list = box_index_list steps leaf in
    let put_into_box idx =
      let old_list = boxgrid.(idx.(0)).(idx.(1)).(idx.(2)) in
      boxgrid.(idx.(0)).(idx.(1)).(idx.(2)) <- (leaf :: old_list) in
    List.iter put_into_box index_list in
  Array.iter put_into_boxes leafarray ;
  boxgrid

let leafarray1 = make_leafarray domain_walls n_leaves 
let boxgrid1 = filled_boxgrid gridsize gridsteps leafarray1 ;;

let () =
  let cnt = ref 0 in
  for i = 0 to nx -. 1 do
    for j = 0 to ny -. 1 do
      for k = 0 to nz -.1 do
	cnt := !cnt +. List.length boxgrid1.(i).(j).(k)
      done
    done
  done ; printf "leaf instances: %d\n" !cnt


(*
let () = test2 leafarray1 10
*)
