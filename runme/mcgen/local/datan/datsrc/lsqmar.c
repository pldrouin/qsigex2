#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double c[1000], x1[1000], x2[1000], x1red[1000], x2red[1000];
} dasv02_;

#define dasv02_1 dasv02_
*/

void LIBFUNC lsqmar_(double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *, double const P_T *), double const P_T *t,
             double const P_T *y, double const P_T *deltay,
             integer const P_T *n, integer const P_T *nr, integer P_T *nred,
             integer P_T * list, double P_T *x, double P_T *cx, double P_T *r,
             double P_T *a, double P_T *scrat, integer P_T *nstep)
{
    /* Table of constant values */

    integer c__1 = 1;
    double c_d0 = 0.;

    /* System generated locals */
    integer a_dim1, a_offset, scrat_dim1, scrat_offset, cx_dim1, cx_offset,
            i__1, i__2, i__3;
    double d__1;

    /* Local variables */
    integer i, k, istep;
    double alam, rmin, r1, r2;
    logical final, new__, ok, covmat;
    double *c, *x1, *x2, *x1red, *x2red;
    unsigned long amem;

    if ((amem = sizeof(double) * *n) > MAXALLOC) {
        MEMERROR("lsqmar_: c (too large)");
        goto f_0;
    }
    c = (double *)MEMALLOC(amem);
    if (!c) {
        MEMERROR("lsqmar_: c");
        goto f_0;
    }
    if ((amem = sizeof(double) * 2 * (*nr + *nred)) > MAXALLOC) {
        MEMERROR("lsqmar_: x1 (too large)");
        goto f_1;
    }
    x1 = (double *)MEMALLOC(amem);
    if (!x1) {
        MEMERROR("lsqmar_: x1");
        goto f_1;
    }
    x2 = x1 + *nr; x1red = x2 + *nr; x2red = x1red + *nred;

    /* Parameter adjustments */
    --deltay;
    --y;
    --t;
    --x;
    --list;
    scrat_dim1 = *nred;
    scrat_offset = scrat_dim1 + 1;
    scrat -= scrat_offset;
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    cx_dim1 = *nred;
    cx_offset = cx_dim1 + 1;
    cx -= cx_offset;

    /* Function Body */
    covmat = TRUE_;
    if (*nstep < 0) {
        covmat = FALSE_;
        *nstep = abs(*nstep);
    }
    if (*nstep < 1) {
        *nstep = 100;
    }
    alam = .001;
/* Initial value of minimum function */
    *r = 0.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
        d__1 = (y[i] - (*userfn)(&x[1], nr, &t[i])) / deltay[i];
        *r += d__1 * d__1;
    }
    if (*nred <= 0) {
        *nstep = 1;
        goto L100;
    }
/* For NR=NRED :  set LIST */
    if (*nr == *nred) {
        i__1 = *nr;
        for (i = 1; i <= i__1; ++i) {
            list[i] = 1;
        }
    }
/* start iteration */
    final = FALSE_;
    i__1 = *nstep;
    for (istep = 1; istep <= i__1; ++istep) {
/* Numerical Derivatives */
        auxdri_(userfn, &x[1], &t[1], n, nr, nred, &list[1], &a[a_offset], &ok);
        if (! ok) {
            *nstep = -3;
            goto L100;
        }
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            i__3 = *nred;
            for (k = 1; k <= i__3; ++k) {
                a[i + k * a_dim1] /= deltay[i];
            }
        }
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            c[i - 1] = (y[i] - (*userfn)(&x[1], nr, &t[i])) / deltay[i];
        }
        if (final) {
/* Final Step */
            mtxsvd_(&a[a_offset], c, x1red, r, n, nred, &c__1, &c_d0, &ok);
            if (! ok) {
                *nstep = -1;
                goto L100;
            }
            mtxzrv_(x1, nr);
            mtxpsv_(x1, x1red, nr, nred, &list[1]);
            mtxadv_(&x[1], x1, &x[1], nr);
/* Compute covariance matrix CX of unknowns */
            if (covmat) {
                auxdri_(userfn, &x[1], &t[1], n, nr, nred, &list[1],
                       &a[a_offset], &ok);
                if (! ok) {
                    *nstep = -3;
                    goto L100;
                }
                i__2 = *n;
                for (i = 1; i <= i__2; ++i) {
                    i__3 = *nred;
                    for (k = 1; k <= i__3; ++k) {
                        a[i + k * a_dim1] /= deltay[i];
                    }
                }
                mtxmat_(&a[a_offset], &a[a_offset], &cx[cx_offset], nred, n,
                        nred);
                mtxchi_(&cx[cx_offset], &scrat[scrat_offset], nred);
            }
            *nstep = istep;
            goto L100;
        }
/* compute minimum function for two values of lambda (ALAM) */
        mtxmar_(&a[a_offset], c, &alam, x1red,
                x2red, n, nred, &c_d0, &ok);
        if (! ok) {
            *nstep = -1;
            goto L100;
        }
        mtxzrv_(x1, nr);
        mtxzrv_(x2, nr);
        mtxpsv_(x1, x1red, nr, nred, &list[1]);
        mtxpsv_(x2, x2red, nr, nred, &list[1]);
        mtxadv_(x1, &x[1], x1, nr);
        mtxadv_(x2, &x[1], x2, nr);
        r1 = 0.;
        r2 = 0.;
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
/* Computing 2nd power */
            d__1 = (y[i] - (*userfn)(x1, nr, &t[i])) / deltay[i];
            r1 += d__1 * d__1;
/* Computing 2nd power */
            d__1 = (y[i] - (*userfn)(x2, nr, &t[i])) / deltay[i];
            r2 += d__1 * d__1;
        }
        if (! ok) {
            *nstep = -1;
            goto L100;
        }
/* evaluate results */
        if (r2 <= *r + 1e-15) {
/* reduce lambda and accept new point */
            alam *= .1;
            mtxcpv_(x2, &x[1], nr);
            rmin = r2;
            new__ = TRUE_;
        } else if (r2 > *r + 1e-15 && r1 <= *r + 1e-15) {
/* keep current value of lambda and accept new point */
            mtxcpv_(x1, &x[1], nr);
            rmin = r1;
            new__ = TRUE_;
        } else {
/* increase lambda and reject new point */
            alam *= 10.;
            new__ = FALSE_;
        }
        if (new__) {
/* test for break-off criterion */
            if ((d__1 = *r - rmin, abs(d__1)) < abs(rmin) * 1e-8 + 1e-15) {
                final = TRUE_;
            } else {
                *r = rmin;
            }
        }
    }
    *nstep = -2;
L100:
    MEMFREE((void *)x1);
f_1:
    MEMFREE((void *)c);
f_0:
    return;
} /* lsqmar_ */

