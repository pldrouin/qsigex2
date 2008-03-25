#include <math.h>
#include "datsrc.h"

/* Common Block Declarations */

#if defined(__TURBOC__)
extern
#endif
struct cmings_ {
    double y[1000];
    integer ny;
} LIBDATA cmings_;

double LIBFUNC mingls_(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;
    double ret_val, d__1, d__2;

    /* Local variables */
    double a, f;
    integer i;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    a = abs(x[2]) * 2.506628275;
    if (a < 1e-10) {
        f = -1e10;
    } else {
        f = log(a);
    }
    ret_val = (double) cmings_.ny * f;
    i__1 = cmings_.ny;
    for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
        d__1 = cmings_.y[i - 1] - x[1];
/* Computing 2nd power */
        d__2 = x[2];
        f = d__1 * d__1 / (d__2 * d__2 * 2.);
        ret_val += f;
    }
    return ret_val;
} /* mingls_ */

