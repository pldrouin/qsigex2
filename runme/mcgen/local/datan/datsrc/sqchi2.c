#include "datsrc.h"

double LIBFUNC sqchi2_(double const P_T *p, integer const P_T *n)
{
    /* Table of constant values */

    integer c__0 = 0;

    /* System generated locals */
    double ret_val;

    /* Local variables */
    double xzero, x0, x1, epsiln;

/* boundary of range */
    if (*p >= 1.) {
        ret_val = 1e10;
    }
    if (*p <= 0.) {
        ret_val = 0.;
    }
/* normal range */
    if (*p < 1. && *p > 0.) {
        x1 = (double) (*n);
        x0 = .5 * x1;
        auxzbr_(&x0, &x1, szchi2_, p, n, &c__0);
        auxzfn_(&x0, &x1, &xzero, szchi2_, p, n, &c__0, &epsiln);
        ret_val = xzero;
    }
    return ret_val;
} /* sqchi2_ */

/* ----------------------------------------------------------------- */
double LIBFUNC szchi2_(double const P_T *x, double const P_T *p,
                      integer const P_T *n, integer const P_T *ndum)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */

/* returns P minus cumulative chisquared distribution of (X,N) */
    ret_val = *p - scchi2_(x, n);
    return ret_val;
} /* szchi2_ */

