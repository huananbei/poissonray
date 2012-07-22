external get_idstring:  unit -> string = "dsfmt_get_idstring_c"
external init_int:      int -> unit    = "dsfmt_gv_init_gen_rand_c"
external genrand_float: float -> float =
  "caml_dsfmt_gv_genrand_close_open_c" "dsfmt_gv_genrand_close_open_c" "float"

(* We cannot make genrand_float of type unit -> float and still use the    *)
(* "float" option to avoid boxing. So we need to supply it with a dummy    *)
(* float argument, 0.0.                                                    *)

let genrand () = genrand_float 0.0;;

(* There are more functions in dSFMT than the interfaces implemented       *)
(* above. Above getrand_float generates a random 64bit float from the      *)
(* interval [0,1). dSFMT would also provide intervals (0,1], (0,0) and     *)
(* [1,2). When initiated with a seed int, the 19937 bit long state of the  *)
(* Mersenne twister is actually initiated by generating random numbers,    *)
(* using the seed, by a simple congruential random number generator. dSFMT *)
(* would also provide a function to initialize the whole state with an     *)
(* input array, and functions to fill arrays with random numbers, in one   *)
(* call. Interface to those is not implemented.                            *)
