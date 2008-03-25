#include <math.h>
#include "datsrc.h"

double LIBFUNC lsqgss_(double const P_T *x, integer const P_T *nr,
               double const P_T *t)
{
    /* System generated locals */
    double ret_val, d__1, d__2;

    /* Local variables */
    double arg;

    /* Parameter adjustments */
    --x;

    /* Function Body */
/* Computing 2nd power */
    d__1 = x[2] - *t;
/* Computing 2nd power */
    d__2 = x[3];
    arg = d__1 * d__1 / (2. * (d__2 * d__2));
    if (arg > 700.) {
        ret_val = 0.;
    } else {
        ret_val = x[1] * exp(-arg);
    }
    return ret_val;
} /* lsqgss_ */

