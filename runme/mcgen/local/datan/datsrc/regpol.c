#include <math.h>
#include <limits.h>
#include <stdlib.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double g[1000];
} dasv04_;

#define dasv04_1 dasv04_
*/

void LIBFUNC regpol_(double const P_T *t, double const P_T *y,
             double const P_T *deltay, integer const P_T *n,
             integer const P_T *nr, double P_T *x, double P_T *b,
             double P_T *a, double P_T *chi2)
{
    /* System generated locals */
    integer b_dim1, b_offset, a_dim1, a_offset, i__1, i__2, i__3, i__4;
    double d__1;

    /* Local variables */
    integer i, j, k;
    double beta, tbar, s, alpha, gamma1, gamma2, sg;
    double *g;
    unsigned long amem;

    if ((amem = sizeof(double) * *n) > MAXALLOC) {
        MEMERROR("regpol_ (too large)");
        goto f_0;
    }
    g = (double *)MEMALLOC(amem);
    if (!g) {
        MEMERROR("regpol_");
        goto f_0;
    }

/* compute weights G and weighted mean TBAR */
    /* Parameter adjustments */
    --deltay;
    --y;
    --t;
    --chi2;
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *nr;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    --x;

    /* Function Body */
    sg = 0.;
    tbar = 0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
        d__1 = deltay[i];
        g[i - 1] = 1. / (d__1 * d__1);
        sg += g[i - 1];
        tbar += g[i - 1] * t[i];
    }
    tbar /= sg;
/* compute B and A for NR=1 */
    b[b_dim1 + 1] = 1. / sqrt(sg);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        a[i + a_dim1] = b[b_dim1 + 1];
    }
/* compute B and A for NR=2 */
    if (*nr >= 2) {
        s = 0.;
        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
            d__1 = t[i] - tbar;
            s += g[i - 1] * (d__1 * d__1);
        }
        b[(b_dim1 << 1) + 2] = 1. / sqrt(s);
        b[b_dim1 + 2] = -b[(b_dim1 << 1) + 2] * tbar;
        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
            a[i + (a_dim1 << 1)] = b[b_dim1 + 2] + b[(b_dim1 << 1) + 2] * t[i];
        }
    }
/* compute B and A for NR greater than 2 */
    if (*nr > 2) {
        i__1 = *nr;
        for (j = 3; j <= i__1; ++j) {
            alpha = 0.;
            beta = 0.;
            gamma2 = 0.;
            i__2 = *n;
            for (i = 1; i <= i__2; ++i) {
/* Computing 2nd power */
                d__1 = a[i + (j - 1) * a_dim1];
                alpha += g[i - 1] * t[i] * (d__1 * d__1);
                beta += g[i - 1] * t[i] * a[i + (j - 1) * a_dim1] *
                        a[i + (j - 2) * a_dim1];
            }
            i__2 = *n;
            for (i = 1; i <= i__2; ++i) {
/* Computing 2nd power */
                d__1 = (t[i] - alpha) * a[i + (j - 1) * a_dim1] -
                       beta * a[i + (j - 2) * a_dim1];
                gamma2 += g[i - 1] * (d__1 * d__1);
            }
            gamma1 = 1. / sqrt(gamma2);
            b[j + b_dim1] = gamma1 * (-alpha * b[j - 1 + b_dim1] -
                            beta * b[j - 2 + b_dim1]);
            if (j >= 4) {
                i__2 = j - 2;
                for (k = 2; k <= i__2; ++k) {
                    b[j + k * b_dim1] = gamma1 * (b[j - 1 + (k - 1) * b_dim1]
                                        - alpha * b[j - 1 + k * b_dim1] -
                                        beta * b[j - 2 + k * b_dim1]);
                }
            }
            b[j + (j - 1) * b_dim1] = gamma1 * (b[j - 1 + (j - 2) * b_dim1] -
                    alpha * b[j - 1 + (j - 1) * b_dim1]);
            b[j + j * b_dim1] = gamma1 * b[j - 1 + (j - 1) * b_dim1];
            i__2 = *n;
            for (i = 1; i <= i__2; ++i) {
                a[i + j * a_dim1] = b[j + b_dim1];
                i__3 = j;
                for (k = 2; k <= i__3; ++k) {
                    i__4 = k - 1;
                    a[i + j * a_dim1] += b[j + k * b_dim1] *
                                         pow(t[i], (double)i__4);
                }
            }
        }
    }
/* compute X and CHI2 */
    i__1 = *nr;
    for (j = 1; j <= i__1; ++j) {
        x[j] = 0.;
        chi2[j] = 0.;
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            x[j] += g[i - 1] * a[i + j * a_dim1] * y[i];
        }
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            s = 0.;
            i__3 = j;
            for (k = 1; k <= i__3; ++k) {
                s += a[i + k * a_dim1] * x[k];
            }
/* Computing 2nd power */
            d__1 = y[i] - s;
            chi2[j] += g[i - 1] * (d__1 * d__1);
        }
    }
    MEMFREE((void *)g);
f_0:
    return;
} /* regpol_ */

