#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Common Block Declarations */
/* COMMON /CS1REG/ is needed for Microsoft FORTRAN compiler */
struct {
    double t[100], y[100], deltay[100], x[10], chi2[10],
           b[100] /* was [10][10] */, *a /* was [100][10] */,
           *xpl /* was [200][10] */, *ypl /* was [200][10] */,
           datsx[100], datcov[100];
} cs1reg;

/* Table of constant values */

static integer c__10 = 10, c__200 = 200;

main(void)
{
    /* Local variables */
    static integer i, j, k, l, m, n, lcapt, nmark, ltx, lty, nws,
                   ncol[10], npol;
    static double d, dat, sigma, t0, dt, scalef;
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    if ((cs1reg.a = (double *)MEMALLOC(sizeof(double)*5000)) == NULL) {
      MEMERROR("s1reg");
      return(0);
    } else {
      cs1reg.xpl = cs1reg.a+1000;
      cs1reg.ypl = cs1reg.xpl+2000;
    }
/* identify program to user */
    printf("%s\n", "Program S1REG simulates data points with errors,");
    printf("%s\n", "performs polynomial regression on them");
    printf("%s\n\n", "and presents data and results graphically.");
/* ask for input */
    printf("%s\n", "Enter number N of data points (1 <= N <= 100):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter SIGMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter number M of terms in polynomial for data (1<=M<=N):");
    printf("%s", "> ");
    scanf("%li", &m);
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
        printf("%s%3li%s\n", "Enter X(", i, " ) for simulation:");
        printf("%s", "> ");
        scanf("%lf", &cs1reg.x[i-1]);
    }
    t0 = -1.;
    dt = 2. / (double) (n - 1);
/* store random numbers following standardized normal in Y */
    rnstnr_(cs1reg.y, &n);
/* loop over N data points */
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        cs1reg.t[i - 1] = t0 + (i - 1) * dt;
        dat = cs1reg.x[0];
        if (m > 1) {
            i__2 = m;
            for (j = 2; j <= i__2; ++j) {
                i__3 = j - 1;
                dat += cs1reg.x[j - 1] * pow(cs1reg.t[i - 1], (double)i__3);
            }
        }
/* Now DAT is exactly given by polynomial. */
/* Add normal error of width SIGMA and store result in Y */
        cs1reg.y[i - 1] = dat + cs1reg.y[i - 1] * sigma;
/* Set error of data point equal to SIGMA */
        cs1reg.deltay[i - 1] = sigma;
    }
/* Perform polynomial regression */
    regpol_(cs1reg.t, cs1reg.y, cs1reg.deltay, &n, &c__10, cs1reg.x,
            cs1reg.b, cs1reg.a, cs1reg.chi2);
/* Output results */
    printf("%s\n", "T");
    for (i = 1; i <= n; ++i) {
        if (i % 10 == 1)
            printf(" ");
        printf("%7.2lf", cs1reg.t[i-1]);
        if (i % 10 == 0)
           printf("\n");
    }
    if (n % 10 != 0)
       printf("\n");
    printf("%s\n", "Y");
    for (i = 1; i <= n; ++i) {
        if (i % 10 == 1)
            printf(" ");
        printf("%7.2lf", cs1reg.y[i-1]);
        if (i % 10 == 0)
           printf("\n");
    }
    if (n % 10 != 0)
       printf("\n");
    printf("%s\n", "DELTAY");
    for (i = 1; i <= n; ++i) {
        if (i % 10 == 1)
            printf(" ");
        printf("%7.2lf", cs1reg.deltay[i-1]);
        if (i % 10 == 0)
           printf("\n");
    }
    if (n % 10 != 0)
       printf("\n");
    printf("%s\n ", "X");
    for (i = 1; i <= 10; ++i) {
        printf("%7.2lf", cs1reg.x[i-1]);
    }
    printf("\n");
    printf("%s\n ", "CHI2");
    for (i = 1; i <= 10; ++i) {
        printf("%7.2lf", cs1reg.chi2[i-1]);
    }
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Enter maximum number of terms in polynomials");
    printf("%s\n", "to be plotted (<= 10):");
    printf("%s", "> ");
    scanf("%li", &npol);
    (void)getchar();
/* set errors along abscissa and covariances to zero for graphics */
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        cs1reg.datsx[i - 1] = 0.;
        cs1reg.datcov[i - 1] = 0.;
    }
/* compute polylines for polynomials of degrees 0,1,2,.... */
    t0 = -1.5;
    dt = .01507537688442211;
    for (i = 1; i <= 200; ++i) {
        i__1 = npol;
        for (j = 1; j <= i__1; ++j) {
            cs1reg.xpl[i + j * 200 - 201] = t0 + (i - 1) * dt;
            cs1reg.ypl[i + j * 200 - 201] = 0.;
            i__2 = j;
            for (l = 1; l <= i__2; ++l) {
                d = cs1reg.b[l - 1];
                if (l > 1) {
                    i__3 = l;
                    for (k = 2; k <= i__3; ++k) {
                        i__4 = k - 1;
                        d += cs1reg.b[l + k * 10 - 11] *
                             pow(cs1reg.xpl[i + j * 200 - 201], (double)i__4);
                    }
                }
                cs1reg.ypl[i + j * 200 - 201] += cs1reg.x[l - 1] * d;
            }
        }
    }
    for (j = 1; j <= 10; ++j) {
        ncol[j - 1] = 4;
    }
/* prepare texts for graphics */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    strncpy(tx, "t", 1);
    ltx = 1;
    strncpy(ty, "y", 1);
    lty = 1;
    strncpy(capt, " ", 1);
    lcapt = 1;
    nmark = 5;
    scalef = .5;
/* call graphics routine for data points and multiple polylines */
    grdtmc_(cs1reg.xpl, cs1reg.ypl, &c__200, &npol, ncol, &nmark, &scalef,
           cs1reg.t, cs1reg.y, cs1reg.datsx, cs1reg.deltay,
           cs1reg.datcov, &n, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
    MEMFREE((void *) cs1reg.a);
    return 0;
} /* main */

