#include <math.h>
#include "datsrc.h"

double LIBFUNC sdstud_(double const P_T *x, integer const P_T *n)
{
    /* Table of constant values */

    double c_dp5 = .5;

    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    double beta, sqan, a, an, an2, arg;

    an = (double) (*n);
    an2 = an * .5;
    sqan = sqrt(an);
    arg = -an2 - .5;
    d__1 = *x * *x / an + 1.;
    a = pow(d__1, arg);
    beta = gbetaf_(&c_dp5, &an2);
    ret_val = a / (beta * sqan);
    return ret_val;
} /* sdstud_ */

