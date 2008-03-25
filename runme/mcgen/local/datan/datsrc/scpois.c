#include "datsrc.h"

double LIBFUNC scpois_(integer const P_T *k, double const P_T *alambd)
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
            ret_val += sdpois_(&j, alambd);
        }
    }
    return ret_val;
} /* scpois_ */

