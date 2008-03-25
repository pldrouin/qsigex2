#include <math.h>
#include "datsrc.h"

double LIBFUNC gbetaf_(double const P_T *z, double const P_T *w)
{
    /* System generated locals */
    double ret_val, d__1;

    if (*w < 1e-8) {
        ret_val = 1e30;
    } else {
        d__1 = *z + *w;
        ret_val = exp(glngam_(z) + glngam_(w) - glngam_(&d__1));
    }
    return ret_val;
} /* gbetaf_ */

