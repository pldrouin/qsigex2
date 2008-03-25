#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double c[1000];
} dasv04_;

#define dasv04_1 dasv04_
*/

void LIBFUNC lsqpol_(double const P_T *t, double const P_T *y,
             double const P_T *deltay, integer const P_T *n, integer P_T *nr,
             double P_T *x, double P_T *cx, double P_T *r, double P_T *a,
             double P_T *scrat, logical P_T *ok)
{
    /* Table of constant values */

    integer c__1 = 1;
    double c_d0 = 0.;

    /* System generated locals */
    integer a_dim1, a_offset, cx_dim1, cx_offset, scrat_dim1, scrat_offset,
            i__1, i__2, i__3;

    /* Local variables */
    integer i, k;
    double *c;
    unsigned long amem;

    *ok = FALSE_;
    if ((amem = sizeof(double) * *n) > MAXALLOC) {
        MEMERROR("lsqpol_: (too large)");
        goto f_0;
    }
    c = (double *)MEMALLOC(amem);
    if (!c) {
        MEMERROR("lsqpol_");
        goto f_0;
    }

/* Set up matrix A' */
    /* Parameter adjustments */
    --deltay;
    --y;
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
            if (k == 1) {
                a[i + k * a_dim1] = -1. / deltay[i];
            } else {
                i__3 = k - 1;
                a[i + k * a_dim1] = -pow(t[i], (double)i__3) / deltay[i];
            }
        }
    }
/* Set up vector C' */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        c[i - 1] = -y[i] / deltay[i];
    }
/* Set up matrix GX */
    mtxmat_(&a[a_offset], &a[a_offset], &cx[cx_offset], nr, n, nr);
/* Determine vector X of unknowns */
    mtxsvd_(&a[a_offset], c, &x[1], r, n, nr, &c__1, &c_d0, ok);
    if (*ok) {
/* Determine covariance matrix CX by inverting GX */
        mtxchi_(&cx[cx_offset], &scrat[scrat_offset], nr);
    }
    MEMFREE((void *)c);
f_0:
    return;
} /* lsqpol_ */

