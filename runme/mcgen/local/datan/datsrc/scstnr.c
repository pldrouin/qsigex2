#include "datsrc.h"

double LIBFUNC scstnr_(double const P_T *x)
{
    /* Table of constant values */

    double c_dp5 = .5;

    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    double f, s, arg;

/* Computing 2nd power */
    d__1 = *x;
    arg = d__1 * d__1 * .5;
    s = 1.;
    if (*x < 0.) {
        s = -1.;
    }
    f = gincgm_(&c_dp5, &arg);
    ret_val = (s * f + 1.) * .5;
    return ret_val;
} /* scstnr_ */

