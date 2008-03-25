#include "datsrc.h"

double LIBFUNC sdnorm_(double const P_T *x, double const P_T *x0,
                      double const P_T *sigma)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double u, s1;

    s1 = 1. / *sigma;
    u = (*x - *x0) * s1;
    ret_val = s1 * sdstnr_(&u);
    return ret_val;
} /* sdnorm_ */

