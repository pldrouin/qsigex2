#include <math.h>
#include "datsrc.h"

double LIBFUNC sdchi2_(double const P_T *x, integer const P_T *n)
{
    /* Table of constant values */

    double c_d2 = 2.;

    /* System generated locals */
    integer i__1;
    double ret_val, d__1;

    /* Local variables */
    double b, c, e;

    if (*x != 0. || *n > 2) {
        d__1 = (double) (*n) * .5;
        b = ggamma_(&d__1) * sqrt(pow(c_d2, (double)*n));
        i__1 = *n - 2;
        c = sqrt(pow(*x, (double)i__1));
        e = exp(-(*x) * .5);
        ret_val = e * c / b;
    } else {
        if (*n == 2) {
            ret_val = .5;
        }
        if (*n == 1) {
            ret_val = 1e10;
        }
    }
    return ret_val;
} /* sdchi2_ */

