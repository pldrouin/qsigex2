#include "datsrc.h"

void LIBFUNC minenc_(double P_T *xa, double P_T *xb, double P_T *xc,
             double P_T *ya, double P_T *yb, double P_T *yc,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), integer P_T *nstep,
             double const P_T *x0, double const P_T *xdir,
             integer const P_T *n)
{
    double xend;
    integer istep, nst;
    double xm, ym, buf;

    /* Parameter adjustments */
    --xdir;
    --x0;

    /* Function Body */
    *ya = minfnd_(xa, &x0[1], &xdir[1], n, userfn);
    *yb = minfnd_(xb, &x0[1], &xdir[1], n, userfn);
    nst = *nstep;
    if (nst <= 0) {
        nst = 1000;
    }
    if (*yb > *ya) {
/* exchange A and B */
        buf = *xa;
        *xa = *xb;
        *xb = buf;
        buf = *ya;
        *ya = *yb;
        *yb = buf;
    }
    *xc = *xb + (*xb - *xa) * 1.618034;
    *yc = minfnd_(xc, &x0[1], &xdir[1], n, userfn);
    istep = 0;
L10:
    if (*yb >= *yc && istep < nst) {
        ++istep;
/* step was still downwards */
        xend = *xb + (*xc - *xb) * 10.;
        minprb_(&xm, xa, ya, xb, yb, xc, yc);
        ym = minfnd_(&xm, &x0[1], &xdir[1], n, userfn);
        if ((xm - *xb) * (*xc - xm) > 0.) {
/* XM is between XB and XC */
            if (ym < *yc) {
/* minimum is between XB and XC */
                *xa = *xb;
                *ya = *yb;
                *xb = xm;
                *yb = ym;
                goto L10;
            } else if (ym > *yb) {
/* minimum is between XA and XM */
                *xc = xm;
                *yc = ym;
                goto L10;
            }
/* there was no minimum, go on */
            xm = *xc + (*xc - *xb) * 1.618034;
        } else if ((*xc - xm) * (xm - xend) > 0.) {
/* XM is between XC and XEND */
            if (ym < *yc) {
                *xb = *xc;
                *xc = xm;
                xm = *xc + (*xc - *xb) * 1.618034;
                *yb = *yc;
                *yc = ym;
            }
        } else if ((xm - xend) * (xend - *xc) >= 0.) {
/* XM is beyond XEND */
            xm = xend;
        } else {
            xm = *xc + (*xc - *xb) * 1.618034;
        }
        *xa = *xb;
        *xb = *xc;
        *xc = xm;
        *ya = *yb;
        *yb = *yc;
        *yc = minfnd_(xc, &x0[1], &xdir[1], n, userfn);
        goto L10;
    }
    if (istep == nst) {
        *nstep = -1;
    } else {
        *nstep = istep;
    }
    return;
} /* minenc_ */

