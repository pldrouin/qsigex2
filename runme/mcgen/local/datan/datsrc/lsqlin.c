#include "datsrc.h"

void LIBFUNC lsqlin_(double const P_T *t, double P_T *c,
             double const P_T *deltay, integer const P_T *n, integer P_T *nr,
             double P_T *x, double P_T *cx, double P_T *r, double P_T *a,
             double P_T *scrat, logical P_T *ok)
{
    /* Table of constant values */

    integer c__1 = 1;
    double c_d0 = 0.;

    /* System generated locals */
    integer a_dim1, a_offset, cx_dim1, cx_offset, scrat_dim1, scrat_offset, 
            i__1, i__2;

    /* Local variables */
    integer i, k;

/* compute matrix A' from A */
    /* Parameter adjustments */
    --deltay;
    --c;
    --t;
    scrat_dim1 = *nr;
    scrat_offset = scrat_dim1 + 1;
    scrat -= scrat_offset;
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    cx_dim1 = *nr;
    cx_offset = cx_dim1 + 1;
    cx -= cx_offset;
    --x;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        i__2 = *nr;
        for (k = 1; k <= i__2; ++k) {
            a[i + k * a_dim1] /= deltay[i];
        }
    }
/* Set up vector C' */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        c[i] /= deltay[i];
    }
/* Set up matrix GX */
    mtxmat_(&a[a_offset], &a[a_offset], &cx[cx_offset], nr, n, nr);
/* Determine vector X of unknowns */
    mtxsvd_(&a[a_offset], &c[1], &x[1], r, n, nr, &c__1, &c_d0, ok);
    if (*ok) {
/* Determine covariance matrix CX by inverting GX */
        mtxchi_(&cx[cx_offset], &scrat[scrat_offset], nr);
    }
    return;
} /* lsqlin_ */

