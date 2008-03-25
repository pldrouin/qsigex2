#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

void LIBFUNC smhsgr_(double const P_T *hist, double const P_T *x0,
                    double const P_T *delx, integer const P_T *nx,
                    char const P_T *tx, integer const P_T *ltx,
                    char const P_T *ty, integer const P_T *lty,
                    char const P_T *capt, integer const P_T *lcapt,
                    integer const P_T *nws)
{
    /* Table of constant values */
    integer c__2 = 2;
    double c_d0 = 0., c_dp1 = .1, c_d1p1 = 1.1, c_dp75 = .75,
           c_dp214 = -.214, c_d1p2 = 1.2, c_dmp1 = -.1, c_dp9 = .9,
           c_d5 = 5., c_d1 = 1.;

    /* System generated locals */
    integer i__1;
    double d__1;

    /* Local variables */
    integer i, m1, n0, n1, n2, perpos;
    double scale, x1, hm;
    char numstr[10];

/* determine scale on ordinate */
    /* Parameter adjustments */
    --hist;

    /* Function Body */
    hm = hist[1];
    i__1 = *nx;
    for (i = 2; i <= i__1; ++i) {
/* Computing MAX */
        d__1 = hist[i];
        hm = max(d__1,hm);
    }
    (void)sprintf(numstr,"%9.1lE",hm);

    perpos = strcspn(numstr, ".");

    m1 = (integer)(numstr[perpos-1]-'0');
 /*
    m2 = (integer)(numstr[3]-'0');
 */
    n0 = 0;
    n1 = m1 + 1;
    n2 = 0;
    if (n1 > 9) {
        n0 = 1;
        n1 = 0;
    }
    if (n0 != 0)
        numstr[perpos-2] = (char)(n0+'0');
    numstr[perpos-1] = (char)(n1+'0');
    numstr[perpos+1] = (char)(n2+'0');
    (void)sscanf(numstr,"%lE",&scale);
/* graphics */
    gropen_();
    gropws_(nws);
    x1 = *x0 + (double) (*nx) * *delx;
    grwncc_(x0, &x1, &c_d0, &scale);
    grvwwc_(&c_dp1, &c_d1p1, &c_dp1, &c_dp75);
    grwnwc_(&c_dp214, &c_d1p2, &c_dmp1, &c_dp9);
    grstfr_(nws, &c_d5, &c_d0);
    grfram_();
    grsclx_(tx, ltx);
    grscly_(ty, lty);
    grtxtc_(&c_d1, capt, lcapt);
    grboun_();
    grstcl_(&c__2);
    grhsdr_(&hist[1], x0, delx, nx);
    grclws_(nws);
    grclse_();
    return;
} /* smhsgr_ */

