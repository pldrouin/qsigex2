#include "datsrc.h"

double LIBFUNC sqnorm_(double const P_T *p, double const P_T *x0,
                      double const P_T *sigma)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double u;

    u = sqstnr_(p);
    ret_val = u * *sigma + *x0;
    return ret_val;
} /* sqnorm_ */

