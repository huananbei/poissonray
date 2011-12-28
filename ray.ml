open Printf;;
open Dsfmt;;

let () = 
  init_int 15;
  printf "%f\n" (genrand ())
