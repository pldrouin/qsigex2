#include "datsrc.h"

double LIBFUNC scftst_(double const P_T *x, integer const P_T *nf1,
                      integer const P_T *nf2)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double a, b, arg;

    arg = (double) (*nf2) / ((double) (*nf2) + (double) (*nf1) * *x);
    a = (double) (*nf2) * .5;
    b = (double) (*nf1) * .5;
    ret_val = 1. - gincbt_(&a, &b, &arg);
    return ret_val;
} /* scftst_ */

