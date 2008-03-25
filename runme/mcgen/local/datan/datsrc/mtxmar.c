#include <stdlib.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double d[1000], e[1000];
} dasv01_;

#define dasv01_1 dasv01_

struct {
    double p1[1000], p2[1000];
} dasv00_;

#define dasv00_1 dasv00_
*/

void LIBFUNC mtxmar_(double P_T *F_P(a), double P_T *b, double const P_T *alam,
             double P_T *x1, double P_T *x2, integer const P_T *m,
             integer P_T *n, double const P_T *frac, logical P_T *ok)
{
    /* Table of constant values */

    integer c__1 = 1;

    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset;

    /* Local variables */
    double *d, *e;
    unsigned long amem;

#if defined(USEHUGE)
    HFP(double,a);
#endif

    *ok = FALSE_;
    if ((amem = sizeof(double) * 2 * *n) > MAXALLOC) {
        MEMERROR("mtxmar_ (too large)");
        goto f_0;
    }
    d = (double *)MEMALLOC(amem);
    if (!d) {
        MEMERROR("mtxmar_");
        goto f_0;
    }
    e = d + *n;

/* STEP 1: Bidiagonalisation of A */
    /* Parameter adjustments */
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    --x2;
    --x1;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    mtxsv1_((double P_T *)&a[a_offset], &b[b_offset], d, e, m, n, &c__1);
/* STEP 2: Diagonalisation of bidiagonal matrix */
    mtxsv2_((double P_T *)&a[a_offset], &b[b_offset], d, e, m, n, &c__1, ok);
/* STEP 3: Order singular values and perform  permutations */
    mtxsv3_((double P_T *)&a[a_offset], &b[b_offset], d, m, n, &c__1);
/* STEP 4: Singular value analysis and appl. of Marquardt method */
    mtxsvm_((double P_T *)&a[a_offset], &b[b_offset], d, alam, &x1[1], &x2[1],
           m, n, frac);
    MEMFREE((void *)d);
f_0:
    return;
} /* mtxmar_ */

/* ----------------------------------------------------------------- */
void LIBFUNC mtxsvm_(double const P_T *F_P(a), double const P_T *b,
             double const P_T *d, double const P_T *alam, double P_T *x1,
             double P_T *x2, integer const P_T *m, integer P_T *n,
             double const P_T *frac)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    double d__1, d__2;

    /* Local variables */
    integer i, k, kk;
    double alam2, g, alamp, fract, alamp2, sinmin, sinmax, den1, den2;
    double *p1, *p2;
    unsigned long amem;

#if defined(USEHUGE)
    HFP(double,a);
#endif

    if ((amem = sizeof(double) * 2 * *m) > MAXALLOC) {
        MEMERROR("mtxmar_/mtxsvm_ (too large)");
        goto f_0;
    }
    p1 = (double *)MEMALLOC(amem);
    if (!p1) {
        MEMERROR("mtxmar_/mtxsvm_");
        goto f_0;
    }
    p2 = p1 + *m;

    /* Parameter adjustments */
    --b;
    --x2;
    --x1;
    --d;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    fract = abs(*frac);
    if (fract < 1e-15) {
        fract = 1e-15;
    }
    sinmax = 0.;
/* Computing 2nd power */
    d__1 = *alam;
    alam2 = d__1 * d__1;
    alamp = *alam * .1;
/* Computing 2nd power */
    d__1 = alamp;
    alamp2 = d__1 * d__1;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* Computing MAX */
        d__1 = sinmax, d__2 = d[i];
        sinmax = max(d__1,d__2);
    }
    sinmin = sinmax * fract;
    kk = *n;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if (d[i] <= sinmin) {
            kk = i - 1;
            goto L30;
        }
    }
L30:
    i__1 = *m;
    for (i = 1; i <= i__1; ++i) {
        g = b[i];
        if (i <= kk) {
/* Computing 2nd power */
            d__1 = d[i];
            den1 = 1.f / (d__1 * d__1 + alam2);
/* Computing 2nd power */
            d__1 = d[i];
            den2 = 1.f / (d__1 * d__1 + alamp2);
            p1[i - 1] = g * d[i] * den1;
            p2[i - 1] = g * d[i] * den2;
        } else {
            if (i <= *n) {
                p1[i - 1] = 0.f;
                p2[i - 1] = 0.f;
            }
        }
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        x1[i] = 0.f;
        x2[i] = 0.f;
        i__2 = *n;
        for (k = 1; k <= i__2; ++k) {
            x1[i] += a[i + k * a_dim1] * p1[k - 1];
            x2[i] += a[i + k * a_dim1] * p2[k - 1];
        }
    }
    *n = kk;
    MEMFREE((void *)p1);
f_0:
    return;
} /* mtxsvm_ */

