#include "datsrc.h"

double LIBFUNC schypg_(integer const P_T *k, integer const P_T *n,
                      integer const P_T *kk, integer const P_T *nn)
{
    /* System generated locals */
    integer i__1;
    double ret_val;

    /* Local variables */
    integer j;

    ret_val = 0.;
    if (*k > 0) {
        i__1 = *k - 1;
        for (j = 0; j <= i__1; ++j) {
            ret_val += sdhypg_(&j, n, kk, nn);
        }
    }
    return ret_val;
} /* schypg_ */

