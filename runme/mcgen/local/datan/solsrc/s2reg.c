#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__10 = 10, c__200 = 200;

main(void)
{
    /* Local variables */
    static integer i, j, m, n, iswit, lcapt, nmark, ltx, lty, nws,
                   ncol[3], npol;
    static double xpl[600] /* was [200][3] */, ypl[600] /* was [200][3] */,
                  a[1000] /* was [100][10] */, b[100] /* was [10][10] */,
                  chi2[10], dat, datcov[100], datsx[100], deltay[100], t0, dt,
                  scalef, confid, coneta[200], t[100], x[10], y[100], sigma;
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1, i__2, i__3;
/* identify program to user */
    printf("%s\n", "Program S2REG simulates data points with errors,");
    printf("%s\n", "performs polynomial regression on them");
    printf("%s\n", "and presents data and results graphically");
    printf("%s\n\n", "including confidence limits.");
/* ask for input */
    printf("%s\n", "Enter number N of data points (10 <= N <= 100):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter SIGMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - SIGMA known for regression");
    printf("%s\n", "2 - SIGMA unknown");
    printf("%s", "> ");
    scanf("%li", &iswit);
    printf("%s\n", "Enter number M of terms in polynomial for data (1<=M<=N):");
    printf("%s", "> ");
    scanf("%li", &m);
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
        printf("%s%3li%s\n", "Enter X(", i, " ) for simulation:");
        printf("%s", "> ");
        scanf("%lf", &x[i-1]);
    }
    t0 = -1.;
    dt = 2. / (double) (n - 1);
/* store random numbers following standardized normal in Y */
    rnstnr_(y, &n);
/* loop over N data points */
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        t[i - 1] = t0 + (i - 1) * dt;
        dat = x[0];
        if (m > 1) {
            i__2 = m;
            for (j = 2; j <= i__2; ++j) {
                i__3 = j - 1;
                dat += x[j - 1] * pow(t[i - 1], (double)i__3);
            }
        }
/* Now DAT is exactly given by polynomial. */
/* Add normal error of width SIGMA and store result in Y */
        y[i - 1] = dat + y[i - 1] * sigma;
/* Set error of data point equal to SIGMA or equal to one */
        if (iswit == 1) {
            deltay[i - 1] = sigma;
        } else {
            deltay[i - 1] = 1.;
        }
    }
/* Perform polynomial regression */
    regpol_(t, y, deltay, &n, &c__10, x, b, a, chi2);
/* Output results */
    printf("%s\n", "T");
    mtxwrt_(t, &c__1, &n);
    printf("%s\n", "Y");
    mtxwrt_(y, &c__1, &n);
    printf("%s\n", "DELTAY");
    mtxwrt_(deltay, &c__1, &n);
    printf("%s\n ", "X");
    mtxwrt_(x, &c__1, &c__10);
    printf("%s\n ", "CHI2");
    mtxwrt_(chi2, &c__1, &c__10);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Enter number n_pol of terms in polynomial:");
    printf("%s\n", "to be plotted");
    printf("%s", "> ");
    scanf("%li", &npol);
    printf("%s\n", "Enter confidence level ( < 1.0 ):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    (void)getchar();
/* perform polynomial regression using NPOL */
    regpol_(t, y, deltay, &n, &npol, x, b, a, chi2);
/* set errors along abscissa and covariances to zero for graphics */
/* in case that errors are unknown, */
/* set also errors along ordinate to zero */
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        datsx[i - 1] = 0.;
        datcov[i - 1] = 0.;
        if (iswit == 2) {
            deltay[i - 1] = 0.;
        }
    }
/* compute polylines for fitted polynomial (I=1) and confidence */
/* limits (I=2,3) using REGCON */
    if (iswit == 1) {
        chi2[npol - 1] = 0.;
    }
    t0 = -1.5;
    dt = .01507537688442211;
    for (i = 1; i <= 200; ++i) {
        xpl[i - 1] = t0 + (i - 1) * dt;
        i__1 = n - npol;
        regcon_(x, b, &xpl[i - 1], &chi2[npol - 1], &confid, &npol, &i__1,
               &ypl[i - 1], &coneta[i - 1]);
        xpl[i + 199] = xpl[i - 1];
        xpl[i + 399] = xpl[i - 1];
        ypl[i + 199] = ypl[i - 1] - coneta[i - 1];
        ypl[i + 399] = ypl[i - 1] + coneta[i - 1];
    }
    ncol[0] = 4;
    ncol[1] = -5;
    ncol[2] = -5;
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    strncpy(tx, "t", 1);
    ltx = 1;
    strncpy(ty, "y", 1);
    lty = 1;
    sprintf(capt, " n_pol# = %3li,   C.L. = %6.4lf", npol, confid);
    lcapt = strlen(capt);
    nmark = 5;
    scalef = .5;
    grdtmc_(xpl, ypl, &c__200, &c__3, ncol, &nmark, &scalef, t, y, datsx,
            deltay, datcov, &n, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
    return 0;
} /* main */

