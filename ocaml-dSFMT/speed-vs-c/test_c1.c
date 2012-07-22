#include <stdio.h>
#include <stdlib.h>
#include "dSFMT.h"

int main() {
    dsfmt_gv_init_gen_rand(12345);
    for (int i = 0; i < 100000000; i++) {
        dsfmt_gv_genrand_close_open();
    }
    return 0;
}
