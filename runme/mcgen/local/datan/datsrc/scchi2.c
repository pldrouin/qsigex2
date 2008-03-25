#include "datsrc.h"

double LIBFUNC scchi2_(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    double a;

    a = (double) (*n) * .5;
    d__1 = *x * .5;
    ret_val = gincgm_(&a, &d__1);
    return ret_val;
} /* scchi2_ */

