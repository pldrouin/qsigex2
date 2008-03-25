#include <stdio.h>
#include <string.h>
#include "grsrc.h"
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__9 = 9;
static double c_dm1 = -1., c_dp1 = .1;

main()
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ndat, npl, i, lcapt, nmark, nws, ltx, lty;
    static double datx[21], daty[21], a, b, datsx[21], datsy[21], datcov[21],
                  scalef, xpl[100], ypl[100];
    static char capt[75], tx[75], ty[75];

/* identify program to user */
    (void)printf("%s\n\n", "Program E7GR demonstrates use of GRDTCV.");
/* ask for number of workstation */
    grnbws_();
    (void)printf("%s\n", "Please, enter number of workstation:");
    (void)printf("%s", "> ");
    (void)scanf("%ld", &nws);
    (void)getchar();
/* generate data points */
    ndat = 21;
    a = 1.;
    b = 1.;
    rnline_(&a, &b, &c_dm1, &c_dp1, &ndat, &c_dp1, datx, daty);
    i__1 = ndat;
    for (i = 1; i <= i__1; ++i) {
	datsx[i - 1] = 0.;
	datsy[i - 1] = .1;
	datcov[i - 1] = 0.;
    }
/* compute points which define polyline */
    npl = 2;
    xpl[0] = -2.;
    ypl[0] = a * xpl[0] + b;
    xpl[1] = 2.;
    ypl[1] = a * xpl[1] + b;
/* prepare texts and caption */
    (void)memset(tx, ' ', 75);
    (void)memset(ty, ' ', 75);
    (void)memset(capt, ' ', 75);
    (void)strncpy(tx, "t", 1);
    ltx = 1;
    (void)strncpy(ty, "y", 1);
    lty = 4;
    (void)strncpy(capt, "y = at + b", 10);
    lcapt = 11;
    nmark = 5;
    scalef = .5;
/* produce graphics by call of GRDTCV */
    grdtcv_(xpl, ypl, &npl, &nmark, &scalef, datx, daty, datsx, datsy, datcov,
	   &ndat, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
    return 0;
} /* main */

