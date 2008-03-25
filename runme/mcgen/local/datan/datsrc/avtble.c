#include <math.h>
#include "datsrc.h"

void LIBFUNC avtble_(double const P_T *x, integer const P_T *ni,
             integer const P_T *nj, integer const P_T *nk, double P_T *xb,
             double P_T *xbi, double P_T *xbj, double P_T *xbij,
             double P_T *q, double P_T *s, double P_T *f, integer P_T *ndf,
             double P_T *a)
{
    /* System generated locals */
    integer x_dim1, x_dim2, x_offset, xbij_dim1, xbij_offset, i__1, i__2, 
            i__3;
    double d__1;

    /* Local variables */
    integer i, j, k, l;
    double df[6], ai, ak, aj;

    /* Parameter adjustments */
    --xbi;
    xbij_dim1 = *ni;
    xbij_offset = xbij_dim1 + 1;
    xbij -= xbij_offset;
    --xbj;
    x_dim1 = *ni;
    x_dim2 = *nj;
    x_offset = x_dim1 * (x_dim2 + 1) + 1;
    x -= x_offset;
    --q;
    --s;
    --f;
    --ndf;
    --a;

    /* Function Body */
    ai = (double) (*ni);
    ak = (double) (*nk);
    aj = (double) (*nj);
/* compute means */
    *xb = 0.;
    i__1 = *ni;
    for (i = 1; i <= i__1; ++i) {
        xbi[i] = 0.;
    }
    i__1 = *nj;
    for (j = 1; j <= i__1; ++j) {
        xbj[j] = 0.;
        i__2 = *ni;
        for (i = 1; i <= i__2; ++i) {
            xbij[i + j * xbij_dim1] = 0.;
        }
    }
    i__2 = *ni;
    for (i = 1; i <= i__2; ++i) {
        i__1 = *nj;
        for (j = 1; j <= i__1; ++j) {
            i__3 = *nk;
            for (k = 1; k <= i__3; ++k) {
                xbij[i + j * xbij_dim1] += x[i + (j + k * x_dim2) * x_dim1];
            }
            xbij[i + j * xbij_dim1] /= ak;
        }
    }
    i__1 = *ni;
    for (i = 1; i <= i__1; ++i) {
        i__2 = *nj;
        for (j = 1; j <= i__2; ++j) {
            xbi[i] += xbij[i + j * xbij_dim1];
        }
        xbi[i] /= aj;
    }
    i__1 = *nj;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *ni;
        for (i = 1; i <= i__2; ++i) {
            xbj[j] += xbij[i + j * xbij_dim1];
        }
        xbj[j] /= ai;
        *xb += xbj[j];
    }
    *xb /= aj;
/* compute sums of squares */
    for (l = 1; l <= 6; ++l) {
        q[l] = 0.;
    }
    i__1 = *ni;
    for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
        d__1 = xbi[i] - *xb;
        q[1] += d__1 * d__1;
    }
    q[1] = aj * ak * q[1];
    i__1 = *nj;
    for (j = 1; j <= i__1; ++j) {
/* Computing 2nd power */
        d__1 = xbj[j] - *xb;
        q[2] += d__1 * d__1;
    }
    q[2] = ai * ak * q[2];
    i__1 = *ni;
    for (i = 1; i <= i__1; ++i) {
        i__2 = *nj;
        for (j = 1; j <= i__2; ++j) {
/* Computing 2nd power */
            d__1 = xbij[i + j * xbij_dim1] + *xb - xbi[i] - xbj[j];
            q[3] += d__1 * d__1;
            i__3 = *nk;
            for (k = 1; k <= i__3; ++k) {
/* Computing 2nd power */
                d__1 = x[i + (j + k * x_dim2) * x_dim1] -
                       xbij[i + j * xbij_dim1];
                q[5] += d__1 * d__1;
/* Computing 2nd power */
                d__1 = x[i + (j + k * x_dim2) * x_dim1] - *xb;
                q[6] += d__1 * d__1;
            }
        }
    }
    q[3] = ak * q[3];
    q[4] = q[2] + q[3];
/* determine degrees of freedom */
    df[0] = ai - 1.;
    df[1] = aj - 1.;
    df[2] = (ai - 1.) * (aj - 1.);
    df[3] = df[1] + df[2];
    df[4] = ai * aj * (ak - 1.);
    df[5] = ai * aj * ak - 1.;
/* mean squares */
    for (l = 1; l <= 6; ++l) {
        s[l] = q[l] / df[l - 1];
    }
/* F quotients */
    for (l = 1; l <= 4; ++l) {
        f[l] = s[l] / s[5];
    }
/* degrees of freedom as integers */
    for (l = 1; l <= 6; ++l) {
        ndf[l] = nint(df[l - 1]);
    }
/* levels of significance */
    for (l = 1; l <= 4; ++l) {
        a[l] = 1. - scftst_(&f[l], &ndf[l], &ndf[5]);
    }
    return;
} /* avtble_ */

