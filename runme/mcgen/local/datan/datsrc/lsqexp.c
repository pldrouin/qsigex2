#include <math.h>
#include "datsrc.h"

double LIBFUNC lsqexp_(double const P_T *x, integer const P_T *nr,
               double const P_T *t)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double arg;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    arg = x[2] * *t;
    if (arg > 700.) {
        ret_val = 0.;
    } else {
        ret_val = x[1] * exp(-arg);
    }
    return ret_val;
} /* lsqexp_ */

