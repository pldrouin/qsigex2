#include <stdlib.h>
#include <limits.h>
#include <math.h>
#include "datsrc.h"

void LIBFUNC minasy_(double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), integer const P_T *n,
             integer const P_T *nred, integer P_T *list, double const P_T *x0,
             double const P_T *cx, double const P_T *fcont, double P_T *dxplus,
             double P_T *dxmins, double P_T *xr, integer P_T *nstep)
{
    /* System generated locals */
    integer xr_dim1, xr_offset, cx_dim1, cx_offset, i__1, i__2;
    double d__1, d__2;

    /* Local variables */
    integer isign, ivar, nred1, i, nstepl;
    double del, sav, dxmin, xivar, r1, xbig, epsi, xsmall, signum;
    logical lencls;
    /*double x[100], dx[100];*/
    double *dx, *x;
    unsigned long amem;

    if ((amem = sizeof(double) * *n) > MAXALLOC) {
        MEMERROR("minasy_: x (too large)");
        goto f_0;
    }
    x = (double *)MEMALLOC(amem);
    if (!x) {
        MEMERROR("minasy_: x");
        goto f_0;
    }
    if ((amem = sizeof(double) * max(*n,*nred)) > MAXALLOC) {
        MEMERROR("minasy_: dx (too large)");
        goto f_1;
    }
    dx = (double *)MEMALLOC(amem);
    if (!dx) {
        MEMERROR("minasy_: dx");
        goto f_1;
    }

    /* Parameter adjustments */
    --x0;
    --list;
    xr_dim1 = *nred;
    xr_offset = xr_dim1 + 1;
    xr -= xr_offset;
    --dxmins;
    --dxplus;
    cx_dim1 = *nred;
    cx_offset = cx_dim1 + 1;
    cx -= cx_offset;

    /* Function Body */
    if (*nstep <= 0) {
        *nstep = 1000;
    }
    if (*n == *nred) {
        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
            list[i] = 1;
        }
    }
    dxmin = 1e20;
    i__1 = *nred;
    for (i = 1; i <= i__1; ++i) {
        dx[i - 1] = sqrt(cx[i + i * cx_dim1]);
/* Computing MIN */
        d__1 = dxmin, d__2 = dx[i - 1];
        dxmin = min(d__1,d__2);
    }
    dxmin = .01 * dxmin;
    nred1 = *nred - 1;
    i__1 = *n;
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
                nstepl = *nstep;
                mtxcpv_(&x0[1], x, n);
                x[ivar - 1] = xivar;
                epsi = 0.;
                r1 = dxmin;
                minsim_(x, n, &nred1, &list[1], userfn, &r1, &epsi, &nstepl,
                        &xr[xr_offset]);
                if (nstepl < 1) {
                    *nstep = -1;
                    goto L70;
                }
/* test for convergence */
                if ((d__1 = r1 - *fcont, abs(d__1)) < .001) {
                    if (isign < 0) {
                        dxmins[ivar] = x0[ivar] - x[ivar - 1];
                    } else {
                        dxplus[ivar] = x[ivar - 1] - x0[ivar];
                    }
                    goto L40;
                } else {
                    if (! lencls) {
/* zero was not yet enclosed in last step */
                        if (r1 > *fcont) {
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
                        if (r1 > *fcont) {
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
} /* minasy_ */

