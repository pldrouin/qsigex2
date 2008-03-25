#include "datsrc.h"

double LIBFUNC sqstnr_(double const P_T *p)
{
    /* Table of constant values */

    integer c__0 = 0;
    double c_d1em8 = 1e-8;

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
        x1 = .1;
        auxzbr_(&x0, &x1, szstnr_, p, &c__0, &c__0);
        auxzfn_(&x0, &x1, &xzero, szstnr_, p, &c__0, &c__0, &c_d1em8);
        ret_val = xzero;
    }
    return ret_val;
} /* sqstnr_ */

/* ----------------------------------------------------------------- */
double LIBFUNC szstnr_(double const P_T *x, double const P_T *p,
                      integer const P_T *ndum1, integer const P_T *ndum2)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */

/* returns P minus cumulative standardized normal of X */
    ret_val = *p - scstnr_(x);
    return ret_val;
} /* szstnr_ */

