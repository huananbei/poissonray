#include <stdio.h>
#include <stdlib.h>
#include "dSFMT.h"

__attribute__ ((noinline))
static double dsfmt_gv_genrand_close_open_c(void)  {
    return dsfmt_gv_genrand_close_open();
    // dsfmt_gv_genrand_close_open will be inlined here
}

int main() {
    dsfmt_gv_init_gen_rand(12345);
    for (int i = 0; i < 100000000; i++) {
        dsfmt_gv_genrand_close_open_c();
    }
    return 0;
}
