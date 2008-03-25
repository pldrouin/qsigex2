#include "datsrc.h"

void LIBFUNC mindir_(double P_T *x0, double const P_T *dir,
             integer const P_T *n,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), double P_T *fmin, integer P_T *nstep,
             double const P_T *epsiln)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i, nstin, nst;
    double xmin, e, xa, xb, xc, ya, yb, yc, dum, eps;

/* initialization */
    /* Parameter adjustments */
    --dir;
    --x0;

    /* Function Body */
    nst = *nstep;
    if (nst <= 0) {
        nst = 1000;
    }
    eps = *epsiln;
    if (eps <= 0.) {
        eps = 1e-8;
    }
    xa = 0.f;
    e = abs(xa);
    if (e < .1) {
        e = .1;
    }
    xb = e * .001;
    nstin = nst;
/* enclose minimum */
    minenc_(&xa, &xb, &xc, &ya, &yb, &yc, userfn, &nst, &x0[1], &dir[1], n);
    if (nst < 0) {
        *nstep = -1;
        goto L20;
    }
    if (xc < xa) {
/* reverse order of arguments */
        dum = xc;
        xc = xa;
        xa = dum;
    }
    nst = nstin;
/* locate minimum */
    mincmb_(&xa, &xc, &eps, userfn, &xmin, fmin, &nst, &x0[1], &dir[1], n);
    if (nst < 0) {
        *nstep = -1;
        goto L20;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        x0[i] += xmin * dir[i];
    }
L20:
    return;
} /* mindir_ */

