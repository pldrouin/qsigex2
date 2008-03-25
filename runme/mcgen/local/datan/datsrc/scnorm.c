#include "datsrc.h"

double LIBFUNC scnorm_(double const P_T *x, double const P_T *x0,
                      double const P_T *sigma)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double u;

    u = (*x - *x0) / *sigma;
    ret_val = scstnr_(&u);
    return ret_val;
} /* scnorm_ */

