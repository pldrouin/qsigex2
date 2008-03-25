#include "datsrc.h"

double LIBFUNC lsqgfn_(double const P_T *eta, double const P_T *x,
               integer const P_T *n, integer const P_T *nr,
               integer const P_T *k)
{
    /* System generated locals */
    double ret_val;

/* computes constraint equations for fit of straight */
/* line to measurements with errors in abscissa and ordinate */
    /* Parameter adjustments */
    --eta;
    --x;

    /* Function Body */
    ret_val = eta[*k * 2] - x[1] - x[2] * eta[(*k << 1) - 1];
    return ret_val;
} /* lsqgfn_ */

