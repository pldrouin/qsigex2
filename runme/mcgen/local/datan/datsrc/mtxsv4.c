#include "datsrc.h"

void LIBFUNC mtxsv4_(double const P_T *F_P(a), double P_T *F_P(b),
                    double const P_T *F_P(d), double P_T *F_P(x),
                    double P_T *F_P(r), integer const P_T *m,
                    integer P_T *n, integer const P_T *nb,
                    double const P_T *frac)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset, i__1, i__2, 
            i__3;
    double d__1, d__2;

    /* Local variables */
    integer i, j, k, kk;
    double fract, s1, sinmin, sinmax;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,d); HFP(double,x); HFP(double,r); 
#endif

    /* Parameter adjustments */
    --d;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --r;
    x_dim1 = *n;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    fract = abs(*frac);
    if (fract < 1e-15) {
        fract = 1e-15;
    }
    sinmax = 0.f;
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
        if (i <= kk) {
            s1 = 1.f / d[i];
            i__2 = *nb;
            for (j = 1; j <= i__2; ++j) {
                b[i + j * b_dim1] *= s1;
            }
        } else {
            i__2 = *nb;
            for (j = 1; j <= i__2; ++j) {
                if (i == kk + 1) {
/* Computing 2nd power */
                    d__1 = b[i + j * b_dim1];
                    r[j] = d__1 * d__1;
                } else {
/* Computing 2nd power */
                    d__1 = b[i + j * b_dim1];
                    r[j] += d__1 * d__1;
                }
                if (i <= *n) {
                    b[i + j * b_dim1] = 0.f;
                }
            }
        }
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        i__2 = *nb;
        for (j = 1; j <= i__2; ++j) {
            x[i + j * x_dim1] = 0.f;
            i__3 = *n;
            for (k = 1; k <= i__3; ++k) {
                x[i + j * x_dim1] += a[i + k * a_dim1] * b[k + j * b_dim1];
            }
        }
    }
    *n = kk;
    return;
} /* mtxsv4_ */

