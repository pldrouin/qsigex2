#include "datsrc.h"

void LIBFUNC rnmngn_(double const P_T *dplus, double const P_T *a,
                    double P_T *x, integer const P_T *n)
{
    /* Table of constant values */

    integer c__1 = 1;

    /* System generated locals */
    integer dplus_dim1, dplus_offset;

    /* Local variables */
    double r[10];

/* generation of random numbers from multivaraiate normal */
    /* Parameter adjustments */
    --x;
    --a;
    dplus_dim1 = *n;
    dplus_offset = dplus_dim1 + 1;
    dplus -= dplus_offset;

    /* Function Body */
    rnstnr_(r, n);
    mtxmlt_(&dplus[dplus_offset], r, &x[1], n, n, &c__1);
    mtxadd_(&x[1], &a[1], &x[1], n, &c__1);
    return;
} /* rnmngn_ */

