#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

void LIBFUNC minsim_(double P_T *x0, integer const P_T *n,
             integer const P_T *nred, integer const P_T *list,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), double P_T *fmin,
             double const P_T *epsiln, integer P_T *nstep, double P_T *x)
{
    /* Table of constant values */

    integer c__1 = 1;
    double c_d2 = 2., c_d1 = 1., c_dm1 = -1., c_dp5 = .5;

    /* System generated locals */
    integer x_dim1, x_offset, i__1, i__2;
    double d__1, d__2;

    /* Local variables */
    integer i, m, ired, istep, ne, mn, mx, nst;
    double d, f, yr, yr2, eps;
    /*double xexp[50], xprime[50], xr[50], xr2[50], xmx[50], xmn[50],
           x1[50], x2[50], y[51];*/
    double *xexp, *xprime, *xr, *xr2, *xmx, *xmn, *x1, *x2, *y;
    unsigned long amem;

    if ((amem = sizeof(double) * (*n + 8 * *nred + 1)) > MAXALLOC) {
        MEMERROR("minsim_ (too large)");
        goto f_0;
    }
    xexp = (double *)MEMALLOC(amem);
    if (!xexp) {
        MEMERROR("minsim_");
        goto f_0;
    }
    xprime = xexp + *n; xr = xprime + *nred; xr2 = xr + *nred;
    xmx = xr2 + *nred; xmn = xmx + *nred; x1 = xmn + *nred; x2 = x1 + *nred;
    y = x2 + *nred;

/* initialization */
    /* Parameter adjustments */
    --list;
    --x0;
    x_dim1 = *nred;
    x_offset = x_dim1 + 1;
    x -= x_offset;

    /* Function Body */
    eps = *epsiln;
    if (eps <= 0.) {
        eps = 1e-25;
    }
    nst = *nstep;
    if (nst <= 0) {
        nst = 1000;
    }
    m = *nred + 1;
/* construct initial simplex and compute function at its corners */
    y[0] = (*userfn)(&x0[1], n);
    mtxgsv_(&x0[1], x1, n, nred, &list[1]);
    mtxpcl_(&x[x_offset], x1, nred, &m, &c__1);
    d = *fmin;
    if (d <= 0.) {
        d = 1e-5;
    }
    ired = 0;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if (list[i] == 1) {
            ++ired;
            mtxcpv_(x1, x2, nred);
            x2[ired - 1] += d;
            i__2 = ired + 1;
            mtxpcl_(&x[x_offset], x2, nred, &m, &i__2);
            mtxcpv_(&x0[1], xexp, n);
            mtxpsv_(xexp, x2, n, nred, &list[1]);
            y[ired] = (*userfn)(xexp, n);
        }
    }
    istep = 0;
/* begin iteration */
L20:
    ++istep;
/* compute indices of corner with largest (MX), next to largest (NE) */
/* and smallest (MN) function value */
    mn = 1;
    if (y[0] > y[1]) {
        mx = 1;
        ne = 2;
    } else {
        mx = 2;
        ne = 1;
    }
    i__1 = *nred + 1;
    for (i = 1; i <= i__1; ++i) {
        if (y[i - 1] < y[mn - 1]) {
            mn = i;
        }
        if (y[i - 1] > y[mx - 1]) {
            ne = mx;
            mx = i;
        } else if (y[i - 1] > y[ne - 1]) {
            if (i != mx) {
                ne = i;
            }
        }
    }
/* test for break-off condition */
    if ((d__1 = y[mx - 1] - y[mn - 1], abs(d__1)) < eps * (d__2 = y[mn - 1],
            abs(d__2)) + 1e-30 || istep > nst) {
        goto L60;
    }
/* compute XPRIME, i.e. point in center of gravity of hyperplane */
/* opposite to point with largest function value */
    mtxgcl_(&x[x_offset], xmx, nred, &m, &mx);
    mtxgcl_(&x[x_offset], xmn, nred, &m, &mn);
    mtxzrv_(xprime, nred);
    i__1 = *nred + 1;
    for (i = 1; i <= i__1; ++i) {
        if (i != mx) {
            mtxgcl_(&x[x_offset], x1, nred, &m, &i);
            mtxadv_(xprime, x1, xprime, nred);
        }
    }
    f = 1. / (double) (*nred);
    mtxmsv_(xprime, xprime, &f, nred);
/* construct points by reflection (XR) and extension (XR2) */
    mtxmsv_(xprime, x1, &c_d2, nred);
    mtxmsv_(xmx, x2, &c_d1, nred);
    mtxsbv_(x1, x2, xr, nred);
    mtxcpv_(&x0[1], xexp, n);
    mtxpsv_(xexp, xr, n, nred, &list[1]);
    yr = (*userfn)(xexp, n);
    if (yr <= y[mn - 1]) {
        mtxmsv_(xr, x1, &c_d2, nred);
        mtxmsv_(xprime, x2, &c_dm1, nred);
        mtxadv_(x1, x2, xr2, nred);
        mtxcpv_(&x0[1], xexp, n);
        mtxpsv_(xexp, xr2, n, nred, &list[1]);
        yr2 = (*userfn)(xexp, n);
        if (yr2 < y[mn - 1]) {
/* perform extension */
            mtxpcl_(&x[x_offset], xr2, nred, &m, &mx);
            y[mx - 1] = yr2;
        } else {
/* perform reflection */
            mtxpcl_(&x[x_offset], xr, nred, &m, &mx);
            y[mx - 1] = yr;
        }
    } else if (yr >= y[ne - 1]) {
        if (yr < y[mx - 1]) {
/* perform reflection */
            mtxpcl_(&x[x_offset], xr, nred, &m, &mx);
            y[mx - 1] = yr;
        }
        mtxmsv_(xmx, x1, &c_dp5, nred);
        mtxmsv_(xprime, x2, &c_dp5, nred);
        mtxadv_(x1, x2, xr2, nred);
        mtxcpv_(&x0[1], xexp, n);
        mtxpsv_(xexp, xr2, n, nred, &list[1]);
        yr2 = (*userfn)(xexp, n);
        if (yr2 < y[mx - 1]) {
/* perform compression */
            mtxpcl_(&x[x_offset], xr2, nred, &m, &mx);
            y[mx - 1] = yr2;
        } else {
/* perform contraction */
            i__1 = *nred + 1;
            for (i = 1; i <= i__1; ++i) {
                if (i != mn) {
                    mtxgcl_(&x[x_offset], x1, nred, &m, &i);
                    mtxadv_(x1, xmn, x2, nred);
                    mtxmsv_(x2, x2, &c_dp5, nred);
                    mtxpcl_(&x[x_offset], x2, nred, &m, &i);
                    mtxcpv_(&x0[1], xexp, n);
                    mtxpsv_(xexp, x2, n, nred, &list[1]);
                    y[i - 1] = (*userfn)(xexp, n);
                }
            }
        }
    } else {
/* perform reflection */
        mtxpcl_(&x[x_offset], xr, nred, &m, &mx);
        y[mx - 1] = yr;
    }
    goto L20;
/* Fill output arguments */
L60:
    --istep;
    *fmin = y[mn - 1];
    mtxgcl_(&x[x_offset], x1, nred, &m, &mn);
    mtxpsv_(&x0[1], x1, n, nred, &list[1]);
    if (istep > nst) {
        *nstep = -1;
    } else {
        *nstep = istep;
    }
    MEMFREE((void *)xexp);
f_0:
    return;
} /* minsim_ */

