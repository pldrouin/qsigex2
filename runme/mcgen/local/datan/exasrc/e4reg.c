#include <stdio.h>
#include <math.h>
#include "datsrc.h"

/* Table of constant values */

    static integer c__1 = 1, c__10 = 10;

main(void)
{
    /* Local variables */
    static integer i, j, m, n;
    static double a[1000] /* was [100][10] */, b[100] /* was [10][10] */,
                  t[100], x[10], y[100], sigma, t0, dt, deltay[100],
                  dat, chi2[10];
    /* System generated locals */
    integer i__1, i__2, i__3;
/* identify program to user */
    printf("%s\n", "Program E4REG simulates data points with errors");
    printf("%s\n\n", "and performs polynomial regression on them.");
/* ask for input */
    printf("%s\n", "Enter number N of data points (10 <= N <=100):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter SIGMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter number M (>0) of terms in polynomial for data:");
    printf("%s", "> ");
    scanf("%li", &m);
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
        printf("%s%3li%s\n", "Enter X(", i, " ):");
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
/* now DAT is exactly given by polynomial. */
/* add normal error of width SIGMA and store result in Y */
        y[i - 1] = dat + y[i - 1] * sigma;
/* set error of data point equal to SIGMA */
        deltay[i - 1] = sigma;
    }
/* perform polynomial regression */
    regpol_(t, y, deltay, &n, &c__10, x, b, a, chi2);
/* output results */
    printf("%s\n", "T");
    mtxwrt_(t, &c__1, &n);
    printf("%s\n", "Y");
    mtxwrt_(y, &c__1, &n);
    printf("%s\n", "DELTAY");
    mtxwrt_(deltay, &c__1, &n);
    printf("%s\n", "X");
    mtxwrt_(x, &c__1, &c__10);
    printf("%s\n", "CHI2");
    mtxwrt_(chi2, &c__1, &c__10);
    return 0;
} /* main */

