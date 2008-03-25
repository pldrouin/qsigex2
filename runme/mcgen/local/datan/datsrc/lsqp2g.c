#include <math.h>
#include "datsrc.h"

double LIBFUNC lsqp2g_(double const P_T *x, integer const P_T *nr,
               double const P_T *t)
{
    /* System generated locals */
    double ret_val, d__1, d__2;

    /* Local variables */
    double back, gauss1, gauss2, arg1, arg2;

    /* Parameter adjustments */
    --x;

    /* Function Body */
/* Computing 2nd power */
    d__1 = *t;
    back = x[1] + x[2] * *t + x[3] * (d__1 * d__1);
/* Computing 2nd power */
    d__1 = x[5] - *t;
/* Computing 2nd power */
    d__2 = x[6];
    arg1 = d__1 * d__1 / (d__2 * d__2 * 2.);
/* Computing 2nd power */
    d__1 = x[8] - *t;
/* Computing 2nd power */
    d__2 = x[9];
    arg2 = d__1 * d__1 / (d__2 * d__2 * 2.);
    if (arg1 > 700.) {
        gauss1 = 0.;
    } else {
        gauss1 = x[4] * exp(-arg1);
    }
    if (arg2 > 700.) {
        gauss2 = 0.;
    } else {
        gauss2 = x[7] * exp(-arg2);
    }
    ret_val = back + gauss1 + gauss2;
    return ret_val;
} /* lsqp2g_ */

