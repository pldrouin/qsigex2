#include <math.h>
#include "datsrc.h"

double LIBFUNC sdpois_(integer const P_T *k, double const P_T *alambd)
{
    /* System generated locals */
    integer i__1;
    double ret_val;

    /* Local variables */
    integer j;

    ret_val = exp(-(*alambd));
    if (*k > 0) {
        i__1 = *k;
        for (j = 1; j <= i__1; ++j) {
            ret_val = ret_val * *alambd / (double) j;
        }
    }
    return ret_val;
} /* sdpois_ */

