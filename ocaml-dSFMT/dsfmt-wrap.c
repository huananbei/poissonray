#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include "dSFMT.h"

CAMLprim value dsfmt_get_idstring_c(value);
CAMLprim value dsfmt_gv_init_gen_rand_c(value);
CAMLprim value caml_dsfmt_gv_genrand_close_open_c(value);
double dsfmt_gv_genrand_close_open_c(double);

CAMLprim value dsfmt_get_idstring_c(value unit) {
    return caml_copy_string(dsfmt_get_idstring());
}

CAMLprim value dsfmt_gv_init_gen_rand_c(value v) {
    dsfmt_gv_init_gen_rand((uint32_t)Int_val(v));
    return Val_unit;
}

CAMLprim value caml_dsfmt_gv_genrand_close_open_c(value v) {
    return caml_copy_double(dsfmt_gv_genrand_close_open());
}

double dsfmt_gv_genrand_close_open_c(double d) {
    return dsfmt_gv_genrand_close_open();
}
