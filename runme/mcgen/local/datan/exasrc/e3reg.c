#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__3 = 3, c__10 = 10, c__200 = 200;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer i, ncol[3], npol, lcapt, nmark, ltx, lty, nws;
    static double t[10] = { -.9,-.7,-.5,-.3,-.1,.1,.3,.5,.7,.9 },
                  y[10] = { 81.,50.,35.,27.,26.,60.,106.,189.,318.,520. },
                  a[100] /* was [10][10] */, b[100] /* was [10][10] */,
                  xpl[600] /* was [200][3] */, ypl[600] /* was [200][3] */,
                  x[10], datsx[10], t0, dt, scalef, confid, coneta[200],
                  datcov[10], deltay[10], chi2[10];
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n\n", "Program E3REG demonstrates use of REGCON.");
/* set errors */
    for (i = 1; i <= 10; ++i) {
        deltay[i - 1] = sqrt(y[i - 1]);
    }
/* perform polynomial regression */
    regpol_(t, y, deltay, &c__10, &c__10, x, b, a, chi2);
/* output results in alphanumeric form */
    printf("%s\n", "T");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", t[i-1]);
    printf("\n");
    printf("%s\n", "Y");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", y[i-1]);
    printf("\n");
    printf("%s\n", "DELTAY");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", deltay[i-1]);
    printf("\n");
    printf("%s\n", "X");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", x[i-1]);
    printf("\n");
    printf("%s\n", "CHI2");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", chi2[i-1]);
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Enter number n_pol of terms in polynomial");
    printf("%s\n", "to be plotted (>0, <10):");
    printf("%s", "> ");
    scanf("%li", &npol);
    printf("%s\n", "Enter confidence level (>0., < 1.0 ):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    (void)getchar();
/* perform polynomial regression */
    regpol_(t, y, deltay, &c__10, &npol, x, b, a, chi2);
/* set errors along abscissa and covariances to zero for graphics */
    for (i = 1; i <= 10; ++i) {
        datsx[i - 1] = 0.;
        datcov[i - 1] = 0.;
    }
/* compute polylines for fitted polynomial (I=1) and confidence */
/* limits (I=2,3) using REGCON */
    t0 = -1.5;
    dt = .01507537688442211;
    for (i = 1; i <= 200; ++i) {
        xpl[i - 1] = t0 + (i - 1) * dt;
        i__1 = 10 - npol;
        regcon_(x, b, &xpl[i - 1], &chi2[npol - 1], &confid, &npol, &i__1,
               &ypl[i - 1], &coneta[i - 1]);
        xpl[i + 199] = xpl[i - 1];
        xpl[i + 399] = xpl[i - 1];
        ypl[i + 199] = ypl[i - 1] - coneta[i - 1];
        ypl[i + 399] = ypl[i - 1] + coneta[i - 1];
    }
/* set colors for different curves */
    ncol[0] = 4;
    ncol[1] = -5;
    ncol[2] = -5;
/* prepare texts and caption */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    tx[0] = 't';
    ltx = 1;
    tx[0] = 'y';
    lty = 1;
    sprintf(capt, " n_pol# = %3li,   C.L. = %6.4lf", npol, confid);
    lcapt = strlen(capt);
    nmark = 5;
    scalef = .5;
/* draw data points and curves */
    grdtmc_(xpl, ypl, &c__200, &c__3, ncol, &nmark, &scalef, t, y, datsx,
            deltay, datcov, &c__10, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
    return 0;
} /* main */

