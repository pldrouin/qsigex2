#include "datsrc.h"

double LIBFUNC scbinm_(integer const P_T *k, integer const P_T *n,
                      double const P_T *p)
{
    /* System generated locals */
    integer i__1;
    double ret_val;

    /* Local variables */
    integer j;
    double s;

    s = 0.;
    if (*k > 0) {
        i__1 = *k - 1;
        for (j = 0; j <= i__1; ++j) {
            s += sdbinm_(&j, n, p);
        }
    }
    ret_val = s;
    return ret_val;
} /* scbinm_ */

