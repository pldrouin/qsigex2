#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double x[1000], dx[1000];
} dasv03_;

#define dasv03_1 dasv03_
*/

void LIBFUNC lsqasm_(double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *, double const P_T *), double const P_T *t,
             double const P_T *y, double const P_T *deltay,
             integer const P_T *n, integer const P_T *nr,
             integer const P_T *nred, integer P_T *list, double const P_T *x0,
             double P_T *cx, double const P_T *r, double const P_T *w,
             double P_T *dxplus, double P_T *dxmins, double P_T *a,
             double P_T *scrat, integer P_T *nstep)
{
    /* System generated locals */
    integer a_dim1, a_offset, scrat_dim1, scrat_offset, cx_dim1, cx_offset,
            i__1, i__2;
    double d__1;

    /* Local variables */
    integer i, isign, ivar, nred1, nstepl;
    double *x, *dx, del, sav, g, rtarg, xivar, r1, xbig, xsmall, signum;
    logical lencls;
    unsigned long amem;

    if ((amem = sizeof(double) * *nr) > MAXALLOC) {
        MEMERROR("lsqasm_: x (too large)");
        goto f_0;
    }
    x = (double *)MEMALLOC(amem);
    if (!x) {
        MEMERROR("lsqasm_: x");
        goto f_0;
    }
    if ((amem = sizeof(double) * max(*nr,*nred)) > MAXALLOC) {
        MEMERROR("lsqasm_: dx (too large)");
        goto f_1;
    }
    dx = (double *)MEMALLOC(amem);
    if (!dx) {
        MEMERROR("lsqasm_: dx");
        goto f_1;
    }
    /* Parameter adjustments */
    --deltay;
    --y;
    --t;
    --x0;
    --list;
    scrat_dim1 = *nred;
    scrat_offset = scrat_dim1 + 1;
    scrat -= scrat_offset;
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --dxmins;
    --dxplus;
    cx_dim1 = *nred;
    cx_offset = cx_dim1 + 1;
    cx -= cx_offset;

    /* Function Body */
    if (*nstep <= 0) {
        *nstep = 100;
    }
    if (*nr == *nred) {
        i__1 = *nr;
        for (i = 1; i <= i__1; ++i) {
            list[i] = 1;
        }
    }
    if (*w <= 1e-4) {
        g = 1.;
    } else {
        g = sqchi2_(w, nred);
    }
    rtarg = *r + g;
    i__1 = *nred;
    for (i = 1; i <= i__1; ++i) {
        dx[i - 1] = sqrt(cx[i + i * cx_dim1]);
    }
    nred1 = *nred - 1;
    i__1 = *nr;
    for (ivar = 1; ivar <= i__1; ++ivar) {
        if (list[ivar] == 0) {
            goto L60;
        }
/* fix variable IVAR */
        list[ivar] = 0;
        sav = x0[ivar];
        for (isign = -1; isign <= 1; isign += 2) {
            signum = (double) isign;
            del = dx[ivar - 1];
            lencls = FALSE_;
/* set XSMALL to x at minimum position */
            xsmall = x0[ivar];
            xivar = x0[ivar] + signum * del;
            i__2 = *nstep;
            for (i = 1; i <= i__2; ++i) {
                nstepl = -(*nstep);
                mtxcpv_(&x0[1], x, nr);
                x[ivar - 1] = xivar;
                lsqmar_(userfn, &t[1], &y[1], &deltay[1], n, nr, &nred1,
                         &list[1], x, &cx[cx_offset], &r1, &a[a_offset],
                         &scrat[scrat_offset], &nstepl);
                if (nstepl < 1) {
                    *nstep = -1;
                    goto L70;
                }
/* test for convergence */
                if ((d__1 = r1 - rtarg, abs(d__1)) < g * .01) {
                    if (isign < 0) {
                        dxmins[ivar] = x0[ivar] - x[ivar - 1];
                    } else {
                        dxplus[ivar] = x[ivar - 1] - x0[ivar];
                    }
                    goto L40;
                } else {
                    if (! lencls) {
/* zero was not yet enclosed in last step */
                        if (r1 > rtarg) {
/* zero is now enclosed, perform first interval halving */
                            lencls = TRUE_;
                            xbig = x[ivar - 1];
                            x[ivar - 1] = (xbig + xsmall) * .5;
                        } else {
/* zero not enclosed, widen range */
                            del *= 2.;
                            x[ivar - 1] = xsmall + signum * del;
                        }
                    } else {
/* continue interval halving */
                        if (r1 > rtarg) {
                            xbig = x[ivar - 1];
                        } else {
                            xsmall = x[ivar - 1];
                        }
                        x[ivar - 1] = (xbig + xsmall) * .5;
                    }
                    xivar = x[ivar - 1];
                }
            }
            *nstep = -2;
            goto L70;
L40:
            ;
        }
/* unfix variable IVAR */
        list[ivar] = 1;
L60:
        ;
    }
L70:
    MEMFREE((void *)dx);
f_1:
    MEMFREE((void *)x);
f_0:
    return;
} /* lsqasm_ */

