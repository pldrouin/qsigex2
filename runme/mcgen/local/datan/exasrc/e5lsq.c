#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__2 = 2, c__20 = 20;
static double c_d0 = 0.;

main(void)
{
    static integer i, nr = 2, nstep, nseed1, nseed2, nred, list[3];
    static double a[60] /* was [20][3] */, cx[9] /* was [3][3] */,
                  gx[9] /* was [3][3] */, deltay[20], r, t[20], x[3], y[20],
                  sigma, dxmins[3], dxplus[3], delt, sig;

/* identify program to user */
    printf("%s\n\n", "Program E5LSQ demonstrates use of LSQASN.");
/* create simulated data */
    x[0] = 10.;
    x[1] = 1.;
    nseed1 = 15;
    nseed2 = 211;
    rne2in_(&nseed1, &nseed2);
    rnstnr_(deltay, &c__20);
    rnecuy_(y, &c__20);
    delt = .09523809523809523;
    sigma = 2.;
    printf("%s\n", "   T         Y       DELTAY");
    for (i = 1; i <= 20; ++i) {
	t[i - 1] = (double) i * delt;
	sig = sigma * (.5 + y[i - 1]);
	y[i - 1] = lsqexp_(x, &c__2, &t[i - 1]) + deltay[i - 1] * sig;
	deltay[i - 1] = sig;
        printf("%10.5lf%10.5lf%10.5lf\n", t[i-1], y[i-1], deltay[i-1]);
    }
    printf("\n");
/* set first approximation of unknowns and perform fit with LSQNON */
    nred = 2;
    x[0] = .1;
    x[1] = .1;
    list[0] = 1;
    list[1] = 1;
    printf("%s%2li%s%2li%s", "NR =", nr, " , NRED = ", nred, " , LIST = ");
    for (i = 1; i <= 3; ++i)
        printf("%2li", list[i-1]);
    printf("%s", ", first appr.:");
    for (i = 1; i <= 3; ++i)
        printf("%8.3lf", x[i-1]);
    printf("\n");
    nstep = 100;
    lsqnon_(lsqexp_, t, y, deltay, &c__20, &c__2, &nred, list, x, cx, &r, a,
           gx, &nstep);
    printf("%s\n", "Result of fit with LSQNON:");
    printf("%s%10.5lf%s%5li\n", "Fit to Exponential, M = ", r,
           " , NSTEP = ", nstep);
    printf("%s", "X = ");
    for (i = 1; i <= nr; ++i)
        printf("%10.5lf", x[i-1]);
    printf("\n");
    printf("%s\n", "Covariance matrix CX =");
    mtxwrt_(cx, &nred, &nred);
    printf("\n");
/* compute asymmetric errors with LSQASN */
    nstep = 100;
    lsqasn_(lsqexp_, t, y, deltay, &c__20, &c__2, &nred, list, x, cx, &r,
           &c_d0, dxplus, dxmins, a, gx, &nstep);
    printf("%s", "DXPLUS = ");
    for (i = 1; i <= nred; ++i)
        printf("%10.5lf", dxplus[i-1]);
    printf("\n");
    printf("%s", "DXMINS = ");
    for (i = 1; i <= nred; ++i)
        printf("%10.5lf", dxmins[i-1]);
    printf("\n");
    return 0;
} /* main */

