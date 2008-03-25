#include "datsrc.h"

double LIBFUNC sqpois_(integer const P_T *k, double const P_T *p)
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
        ret_val = 0.;
    }
/* normal range */
    if (*p < 1. && *p > 0.) {
        x0 = 0.;
        x1 = (double) (*k);
        auxzbr_(&x0, &x1, szpois_, p, k, &c__0);
        auxzfn_(&x0, &x1, &xzero, szpois_, p, k, &c__0, &c_d1em8);
        ret_val = xzero;
    }
    return ret_val;
} /* sqpois_ */

/* ----------------------------------------------------------------- */
double LIBFUNC szpois_(double const P_T *alambd, double const P_T *p,
                      integer const P_T *k, integer const P_T *ndum2)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */

/* returns P minus cumulative Poisson */
    ret_val = *p - scpois_(k, alambd);
    return ret_val;
} /* szpois_ */

