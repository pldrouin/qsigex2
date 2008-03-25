#include <math.h>
#include "datsrc.h"

double LIBFUNC lsq2ex_(double const P_T *x, integer const P_T *nr,
               double const P_T *t)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double e1, e2, arg;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    arg = x[2] * *t;
    if (arg > 700.) {
        e1 = 0.;
    } else {
        e1 = x[1] * exp(-arg);
    }
    arg = x[4] * *t;
    if (arg > 700.) {
        e2 = 0.;
    } else {
        e2 = x[3] * exp(-arg);
    }
    ret_val = e1 + e2;
    return ret_val;
} /* lsq2ex_ */

