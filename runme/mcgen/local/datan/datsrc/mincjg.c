#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

void LIBFUNC mincjg_(double P_T *x0, integer const P_T *n,
             integer const P_T *nred, integer const P_T *list,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), double P_T *fmin,
             double const P_T *epsiln, integer P_T *nstep)
{
    /* Table of constant values */

    double c_dm1 = -1.;
    double c_d0 = 0.;

    /* System generated locals */
    integer i__1;
    double d__1;

    /* Local variables */
    integer istep, lstep, nst;
    double a, gamma, s, fminl, eps;
    /*double gr[50], d[50], g[50], h[50];*/
    double *d, *g, *h, *gr;
    unsigned long amem;

    if ((amem = sizeof(double) * 4 * *n) > MAXALLOC) {
        MEMERROR("mincjg_ (too large)");
        goto f_0;
    }
    d = (double *)MEMALLOC(amem);
    if (!d) {
        MEMERROR("mincjg_");
        goto f_0;
    }
    g = d + *n; h = g + *n; gr = h + *n;

    /* Parameter adjustments */
    --list;
    --x0;

    /* Function Body */
    if (*nstep < 1) {
        *nstep = 1000;
    }
    eps = *epsiln;
    if (eps <= 0.) {
        eps = 1e-8;
    }
/* initialization : directions G and H identical to gradient at X0 */
    fminl = (*userfn)(&x0[1], n);
    if (*nred <= 0) {
/* all variables are fixed */
        *fmin = fminl;
        *nstep = 1;
        goto L40;
    }
    auxgrd_(&x0[1], gr, n, nred, &list[1], userfn);
    mtxmsv_(gr, g, &c_dm1, n);
    mtxcpv_(g, h, n);
/* start iteration */
    i__1 = *nstep;
    for (istep = 1; istep <= i__1; ++istep) {
        lstep = istep;
        if (istep > 1) {
            if ((d__1 = fminl - *fmin, abs(d__1)) < eps * abs(*fmin) + 1e-15) {
                *nstep = istep;
                goto L30;
            }
            fminl = *fmin;
        }
        nst = *nstep;
/* X0 is position of minimum along direction H */
        mindir_(&x0[1], h, n, userfn, fmin, &nst, &c_d0);
        if (nst < 0) {
            goto L20;
        }
        auxgrd_(&x0[1], gr, n, nred, &list[1], userfn);
/* GR is gradient at X0 */
/* compute next set of directions G and H */
        mtxmsv_(gr, gr, &c_dm1, n);
        mtxsbv_(gr, g, d, n);
        mtxdot_(d, gr, &s, n);
        mtxdot_(g, g, &a, n);
        if (a == 0.) {
            goto L30;
        }
        gamma = s / a;
        mtxmsv_(h, h, &gamma, n);
        mtxadv_(gr, h, h, n);
        mtxcpv_(gr, g, n);
    }
L20:
    *nstep = -1;
    goto L40;
L30:
    *nstep = lstep;
L40:
    MEMFREE((void *)d);
f_0:
    return;
} /* mincjg_ */

