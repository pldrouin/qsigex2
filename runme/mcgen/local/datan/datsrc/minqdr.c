#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

void LIBFUNC minqdr_(double P_T *x0, integer const P_T *n, integer P_T *nred,
             integer const P_T *list,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), double P_T *fmin, double const P_T *epsiln,
             integer P_T *nstep, double P_T *hesse)
{
    /* Table of constant values */

    integer c__1 = 1;
    double c_d0 = 0.;

    /* System generated locals */
    integer hesse_dim1, hesse_offset, i__1;
    double d__1;

    /* Local variables */
    integer istep;
    double alam, r, fminl, eps;
    logical ok;
    /*double grad[50], x1red[50], x1[50];*/
    double *grad, *x1red, *x1;
    unsigned long amem;

    if ((amem = sizeof(double) * 3 * *n) > MAXALLOC) {
        MEMERROR("minqdr_ (too large)");
        goto f_0;
    }
    grad = (double *)MEMALLOC(amem);
    if (!grad) {
        MEMERROR("minqdr_");
        goto f_0;
    }
    x1red = grad + *n; x1 = x1red + *n;

    /* Parameter adjustments */
    --list;
    --x0;
    hesse_dim1 = *nred;
    hesse_offset = hesse_dim1 + 1;
    hesse -= hesse_offset;

    /* Function Body */
    ok = TRUE_;
    if (*nstep < 1) {
        *nstep = 100;
    }
    eps = *epsiln;
    if (eps <= 0.) {
        eps = 1e-8;
    }
    alam = .001;
/* initial value of minimum function */
    fminl = (*userfn)(&x0[1], n);
    if (*nred <= 0) {
        *fmin = fminl;
        *nstep = 1;
        goto L20;
    }
/* start iteration */
    i__1 = *nstep;
    for (istep = 1; istep <= i__1; ++istep) {
        auxgrd_(&x0[1], grad, n, nred, &list[1], userfn);
        auxhes_(&x0[1], &hesse[hesse_offset], n, nred, &list[1], userfn)
                ;
/* compute minimum function for two values of lambda (ALAM) */
        mtxsvd_(&hesse[hesse_offset], grad, x1red, &r, nred, nred, &c__1,
               &c_d0, &ok);
        if (! ok) {
            *nstep = -4;
            goto L20;
        }
        mtxzrv_(x1, n);
        mtxpsv_(x1, x1red, n, nred, &list[1]);
        mtxsbv_(&x0[1], x1, x1, n);
        *fmin = (*userfn)(x1, n);
/* test for break-off criterion */
        if ((d__1 = fminl - *fmin, abs(d__1)) < eps * abs(*fmin) + 1e-15) {
            *nstep = istep;
            goto L20;
        }
        mtxcpv_(x1, &x0[1], n);
        fminl = *fmin;
    }
    *nstep = -1;
L20:
    MEMFREE((void *)grad);
f_0:
    return;
} /* minqdr_ */

