#include <stdio.h>
#include "datsrc.h"
#include "grsrc.h"


void LIBFUNC smsdgr_(double const P_T *x, double const P_T *y,
             integer const P_T *n, double const P_T *xa,
             double const P_T *xb, double const P_T *ya,
             double const P_T *yb, char const P_T *tx,
             integer const P_T *ltx, char const P_T *ty,
             integer const P_T *lty, char const P_T *capt,
             integer const P_T *lcapt, integer const P_T *nws)
{
    /* Table of constant values */
    static integer c__1 = 1, c__2 = 2;
    static double c_dp2 = .2, c_dp85 = .85, c_dp1 = .1, c_dp75 = .75,
                  c_dmp214 = -.214, c_d1p2 = 1.2, c_dmp1 = -.1, c_dp9 = .9,
                  c_d5 = 5., c_d0 = 0., c_d1 = 1.;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i;

    /* Parameter adjustments */
    --y;
    --x;

    /* Function Body */
    gropen_();
    gropws_(nws);
    grwncc_(xa, xb, ya, yb);
    grvwwc_(&c_dp2, &c_dp85, &c_dp1, &c_dp75);
    grwnwc_(&c_dmp214, &c_d1p2, &c_dmp1, &c_dp9);
    grstfr_(nws, &c_d5, &c_d0);
    grfram_();
    grsclx_(tx, ltx);
    grscly_(ty, lty);
    grtxtc_(&c_d1, capt, lcapt);
    grboun_();
    grstcl_(&c__2);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        grdatp_(&c__1, &c_dp1, &x[i], &y[i], &c_d0, &c_d0, &c_d0);
    }
    grclws_(nws);
    grclse_();
    return;
} /* smsdgr_ */

