#include "datsrc.h"

void LIBFUNC rnline_(double const P_T *a, double const P_T *b,
             double const P_T *t0, double const P_T *dt,
             integer const P_T *n, double const P_T *sigmay,
             double P_T *t, double P_T *y)
{
    /* Table of constant values */

    integer c__1 = 1;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i;
    double r;

    /* Parameter adjustments */
    --y;
    --t;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        t[i] = *t0 + (i - 1) * *dt;
        y[i] = *a * t[i] + *b;
        rnstnr_(&r, &c__1);
        y[i] += r * *sigmay;
    }
    return;
} /* rnline_ */

