#include <math.h>
#include "datsrc.h"

void LIBFUNC mtxsv2_(double P_T *F_P(a), double P_T *F_P(b), double P_T *F_P(d),
             double P_T *F_P(e), integer const P_T *m, integer const P_T *n,
             integer const P_T *nb, logical P_T *ok)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1;
    double d__1, d__2, d__3;

    /* Local variables */
    integer i, j, k, l, niter, ll, niterm;
    double bmx;
    logical elzero;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,d); HFP(double,e);
#endif

    /* Parameter adjustments */
    --e;
    --d;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    *ok = TRUE_;
    niterm = *n * 10;
    niter = 0;
    bmx = d[1];
    if (*n > 1) {
        i__1 = *n;
        for (i = 2; i <= i__1; ++i) {
/* Computing MAX */
            d__3 = (d__1 = d[i], abs(d__1)) + (d__2 = e[i], abs(d__2));
            bmx = max(d__3,bmx);
        }
    }
    for (k = *n; k >= 1; --k) {
L20:
        if (k != 1) {
            if (bmx + d[k] - bmx == 0.) {
/* Since D(K).EQ.0. perform Givens transform with result E(K)=0. */
                mtxs21_((double P_T *)&a[a_offset], (double P_T *)&d[1],
                       (double P_T *)&e[1], m, n, nb, &k);
            }
/* Find L (2. LE. L .LE. K) so that either E(L)=0. or D(L-1)=0. */
/* In the latter case transform E(L) to zero. In both cases the */
/* matrix splits and the bottom right minor begins with row L. */
/* If no such L is found set L=1 */
            for (ll = k; ll >= 1; --ll) {
                l = ll;
                if (l == 1) {
                    elzero = FALSE_;
                    goto L40;
                } else if (bmx - e[l] - bmx == 0.) {
                    elzero = TRUE_;
                    goto L40;
                } else if (bmx + d[l - 1] - bmx == 0.) {
                    elzero = FALSE_;
                }
            }
L40:
            if (l > 1 && ! elzero) {
                mtxs22_((double P_T *)&b[b_offset], (double P_T *)&d[1],
                       (double P_T *)&e[1], m, n, nb, &k, &l);
            }
            if (l != k) {
/* one more QR pass with order K */
                mtxs23_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset],
                       (double P_T *)&d[1], (double P_T *)&e[1], m, n, nb,
                       &k, &l);
                ++niter;
                if (niter <= niterm) {
                    goto L20;
                }
/* set flag indicating non-convergence */
                *ok = FALSE_;
            }
        }
        if (d[k] < 0.f) {
/* for negative singular values perform change of sign */
            d[k] = -d[k];
            i__1 = *n;
            for (j = 1; j <= i__1; ++j) {
                a[j + k * a_dim1] = -a[j + k * a_dim1];
            }
        }
/* order is decreased by one in next pass */
    }
    return;
} /* mtxsv2_ */

/* ----------------------------------------------------------------- */
void LIBFUNC mtxs21_(double P_T *F_P(a), double P_T *F_P(d), double P_T *F_P(e),
             integer const P_T *m, integer const P_T *n, integer const P_T *nb,
             integer const P_T *k)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;

    /* Local variables */
    integer i, j;
    double h, cs, sn;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,d); HFP(double,e);
#endif

    /* Parameter adjustments */
    --e;
    --d;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    for (i = *k - 1; i >= 1; --i) {
        if (i == *k - 1) {
            mtxgva_((double P_T *)&d[i], (double P_T *)&e[i + 1], &cs, &sn);
        } else {
            mtxgva_((double P_T *)&d[i], &h, &cs, &sn);
        }
        if (i > 1) {
            h = 0.f;
            mtxgvt_((double P_T *)&e[i], &h, &cs, &sn);
        }
        i__1 = *n;
        for (j = 1; j <= i__1; ++j) {
            mtxgvt_((double P_T *)&a[j + i * a_dim1],
                   (double P_T *)&a[j + *k * a_dim1], &cs, &sn);
        }
    }
    return;
} /* mtxs21_ */

/* ----------------------------------------------------------------- */
void LIBFUNC mtxs22_(double const P_T *F_P(b), double P_T *F_P(d),
                    double P_T *F_P(e), integer const P_T *m,
                    integer const P_T *n, integer const P_T *nb,
                    integer const P_T *k, integer const P_T *l)
{
    /* System generated locals */
    integer b_dim1, b_offset, i__1, i__2;

    /* Local variables */
    integer i, j;
    double h, cs, sn;

#if defined(USEHUGE)
    HFP(double,b); HFP(double,d); HFP(double,e);
#endif

    /* Parameter adjustments */
    --e;
    --d;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    i__1 = *k;
    for (i = *l; i <= i__1; ++i) {
        if (i == *l) {
            mtxgva_((double P_T *)&d[i], (double P_T *)&e[i], &cs, &sn);
        } else {
            mtxgva_((double P_T *)&d[i], &h, &cs, &sn);
        }
        if (i < *k) {
            h = 0.f;
            mtxgvt_((double P_T *)&e[i + 1], &h, &cs, &sn);
        }
        i__2 = *nb;
        for (j = 1; j <= i__2; ++j) {
            mtxgvt_(&cs, &sn, (double P_T *)&b[i + j * b_dim1],
                   (double P_T *)&b[*l - 1 + j * b_dim1]);
        }
    }
    return;
} /* mtxs22_ */

/* ----------------------------------------------------------------- */
void LIBFUNC mtxs23_(double P_T *F_P(a), double P_T *F_P(b), double P_T *F_P(d),
             double P_T *F_P(e), integer const P_T *m, integer const P_T *n,
             integer const P_T *nb, integer const P_T *k, integer const P_T *l)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;

    /* Local variables */
    integer i, j;
    double f, g, h, t, cs, sn;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,d); HFP(double,e);
#endif

/* Determine shift parameter */
    /* Parameter adjustments */
    --e;
    --d;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    f = ((d[*k - 1] - d[*k]) * (d[*k - 1] + d[*k]) + (e[*k - 1] - e[*k]) *
         (e[*k - 1] + e[*k])) / (e[*k] * 2.f * d[*k - 1]);
    if (abs(f) > 1e10f) {
        g = abs(f);
    } else {
        g = sqrt(f * f + 1.f);
    }
    if (f >= 0.f) {
        t = f + g;
    } else {
        t = f - g;
    }
    f = ((d[*l] - d[*k]) * (d[*l] + d[*k]) + e[*k] * (d[*k - 1] / t - e[*k])) 
            / d[*l];
    i__1 = *k - 1;
    for (i = *l; i <= i__1; ++i) {
        if (i == *l) {
/* Define R(L) */
            mtxgvd_(&f, (double P_T *)&e[i + 1], &cs, &sn);
        } else {
/* Define R(I) , I.NE.L */
            mtxgva_((double P_T *)&e[i], &h, &cs, &sn);
        }
        mtxgvt_((double P_T *)&d[i], (double P_T *)&e[i + 1], &cs, &sn);
        h = 0.f;
        mtxgvt_(&h, (double P_T *)&d[i + 1], &cs, &sn);
        i__2 = *n;
        for (j = 1; j <= i__2; ++j) {
            mtxgvt_((double P_T *)&a[j + i * a_dim1],
                   (double P_T *)&a[j + (i + 1) * a_dim1], &cs, &sn);
        }
/* Define T(I) */
        mtxgva_((double P_T *)&d[i], &h, &cs, &sn);
        mtxgvt_((double P_T *)&e[i + 1], (double P_T *)&d[i + 1], &cs, &sn);
        if (i < *k - 1) {
            h = 0.f;
            mtxgvt_(&h, (double P_T *)&e[i + 2], &cs, &sn);
        }
        i__2 = *nb;
        for (j = 1; j <= i__2; ++j) {
            mtxgvt_((double P_T *)&b[i + j * b_dim1],
                   (double P_T *)&b[i + 1 + j * b_dim1], &cs, &sn);
        }
    }
    return;
} /* mtxs23_ */

