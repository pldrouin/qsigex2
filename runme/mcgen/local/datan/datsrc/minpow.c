#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

void LIBFUNC minpow_(double P_T *x0, integer const P_T *n,
             integer const P_T *nred, integer const P_T *list,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), double P_T *fmin,
             double const P_T *epsiln, integer P_T *nstep, double P_T *alldir)
{
    /* Table of constant values */

    double c_d0 = 0.;

    /* System generated locals */
    integer alldir_dim1, alldir_offset, i__1, i__2;
    double d__1, d__2;

    /* Local variables */
    integer i, istep, lstep, imax, ired, nst, nstdir;
    double eps, fe, fl, deltaf, fold, test, d, f;
    /*double dirn[50], xold[50], xe[50], dir[50];*/
    double *dir, *dirn, *xe, *xold;
    unsigned long amem;

    if ((amem = sizeof(double) * 5 * *n) > MAXALLOC) {
        MEMERROR("minpow_ (too large)");
        goto f_0;
    }
    dir = (double *)MEMALLOC(amem);
    if (!dir) {
        MEMERROR("minpow_");
        goto f_0;
    }
    dirn = dir + *n; xe = dirn + *n; xold = xe + *n;

    /* Parameter adjustments */
    --list;
    --x0;
    alldir_dim1 = *n;
    alldir_offset = alldir_dim1 + 1;
    alldir -= alldir_offset;

    /* Function Body */
    if (*nstep < 1) {
        *nstep = 1000;
    }
    eps = *epsiln;
    if (eps <= 0.f) {
        eps = 1e-8;
    }
/* Initialize directions along coordinates */
    f = (*userfn)(&x0[1], n);
    ired = 0;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if (list[i] == 1) {
            ++ired;
            mtxzrv_(dir, n);
            dir[i - 1] = 1.;
            mtxpcl_(&alldir[alldir_offset], dir, n, nred, &ired);
        }
    }
    i__1 = *nstep;
    for (istep = 1; istep <= i__1; ++istep) {
        lstep = istep;
        fold = f;
        fl = fold;
        mtxcpv_(&x0[1], xold, n);
        deltaf = 0.f;
        imax = 0;
/* Loop over complete direction set and find direction which */
/* yields largest function decrease. Its index is IMAX */
        i__2 = *nred;
        for (i = 1; i <= i__2; ++i) {
            mtxgcl_(&alldir[alldir_offset], dir, n, nred, &i);
            nstdir = *nstep;
            mindir_(&x0[1], dir, n, userfn, &f, &nstdir, &c_d0);
            if (nstdir < 0) {
                goto L40;
            }
            d = (d__1 = f - fl, abs(d__1));
            if (d > deltaf) {
                deltaf = d;
                imax = i;
            }
            fl = f;
        }
/* Test for break-off criterion */
        if ((d__1 = fl - fold, abs(d__1)) < eps * abs(fl) + 1e-15) {
            goto L50;
        }
/* Construct extrapolated point XE and direction DIRN from */
/* XOLD to X0 */
        mtxsbv_(&x0[1], xold, dirn, n);
        mtxadv_(dirn, &x0[1], xe, n);
        fe = (*userfn)(xe, n);
/* Now there are 3 points (XOLD, X0, XE) */
/* and their function values (FOLD, F, FE) */
        if (fe < f) {
/* Computing 2nd power */
            d__1 = fold - f - deltaf;
/* Computing 2nd power */
            d__2 = fold - fe;
            test = (fold - f * 2.f + fe) * 2.f * (d__1 * d__1) - d__2 * d__2 *
                     deltaf;
            if (test < 0.f) {
                nst = *nstep;
/* Find minimum along DIRN and replace X0 by position */
/* of minimum. Replace direction with index IMAX by DIRN */
                mindir_(&x0[1], dirn, n, userfn, &f, &nst, &c_d0);
                if (nst < 0) {
                    goto L40;
                }
                mtxpcl_(&alldir[alldir_offset], dirn, n, nred, &imax);
            }
        }
    }
L40:
    *nstep = -1;
    goto L60;
L50:
    *nstep = lstep;
    *fmin = fl;
L60:
    MEMFREE((void *)dir);
f_0:
    return;
} /* minpow_ */

