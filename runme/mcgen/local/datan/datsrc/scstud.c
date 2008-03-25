#include "datsrc.h"

double LIBFUNC scstud_(double const P_T *x, integer const P_T *n)
{
    /* Table of constant values */

    double c_dp5 = .5;

    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    double a, an, an2, arg;

    an = (double) (*n);
    an2 = an * .5;
/* Computing 2nd power */
    d__1 = *x;
    arg = an / (an + d__1 * d__1);
    a = gincbt_(&an2, &c_dp5, &arg);
    if (*x >= 0.) {
        ret_val = 1. - a * .5;
    } else {
        ret_val = a * .5;
    }
    return ret_val;
} /* scstud_ */

