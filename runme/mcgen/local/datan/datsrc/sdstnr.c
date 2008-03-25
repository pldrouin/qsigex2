#include <math.h>
#include "datsrc.h"

double LIBFUNC sdstnr_(double const P_T *x)
{
    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    double arg;

/* Computing 2nd power */
    d__1 = *x;
    arg = d__1 * d__1 * -.5;
    if (abs(arg) >= 500.) {
        ret_val = 0.;
    } else {
        ret_val = exp(arg) * .39894228;
    }
    return ret_val;
} /* sdstnr_ */

