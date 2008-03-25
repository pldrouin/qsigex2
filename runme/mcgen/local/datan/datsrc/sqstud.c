#include "datsrc.h"

double LIBFUNC sqstud_(double const P_T *p, integer const P_T *n)
{
    /* Table of constant values */

    integer c__0 = 0;
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
        ret_val = -1e10;
    }
/* normal range */
    if (*p < 1. && *p > 0.) {
        x0 = 0.;
        x1 = *p;
        auxzbr_(&x0, &x1, szstud_, p, n, &c__0);
        auxzfn_(&x0, &x1, &xzero, szstud_, p, n, &c__0, &c_d1em6);
        ret_val = xzero;
    }
    return ret_val;
} /* sqstud_ */

/* ----------------------------------------------------------------- */
double LIBFUNC szstud_(double const P_T *x, double const P_T *p,
                      integer const P_T *n, integer const P_T *ndum)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */

/* returns P minus cumulative Student's distribution of (X,N) */
    ret_val = *p - scstud_(x, n);
    return ret_val;
} /* szstud_ */

