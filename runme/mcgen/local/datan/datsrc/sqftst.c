#include "datsrc.h"
 
double LIBFUNC sqftst_(double const P_T *p, integer const P_T *n1,
                      integer const P_T *n2)
{
    /* Table of constant values */

    double c_d1em6 = 1e-6;

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double xzero, x0, x1;

/* boundary of range */
    if (*p >= 1.) {
        ret_val = 1e10;
    }
    if (*p <= 0.) {
        ret_val = 0.;
    }
/* normal range */
    x0 = 0.;
    x1 = *p;
    if (*p < 1. && *p > 0.) {
        auxzbr_(&x0, &x1, szftst_, p, n1, n2);
        auxzfn_(&x0, &x1, &xzero, szftst_, p, n1, n2, &c_d1em6);
        ret_val = xzero;
    }
    return ret_val;
} /* sqftst_ */

/* ----------------------------------------------------------------- */
double LIBFUNC szftst_(double const P_T *x, double const P_T *p,
                      integer const P_T *n1, integer const P_T *n2)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */

/* returns P minus cumulative F-distribution of (X,N1,N2) */
    ret_val = *p - scftst_(x, n1, n2);
    return ret_val;
} /* szftst_ */

