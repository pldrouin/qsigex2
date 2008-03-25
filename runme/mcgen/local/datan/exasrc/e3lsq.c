#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__3 = 3, c__20 = 20;

main(void)
{
    static integer i, k, nstep, nseed1, nseed2, nloop, nred, nr = 3, list[3];
    static double a[60] /* was [20][3] */, cx[9] /* was [3][3] */,
                  gx[9] /* was [3][3] */, r, t[20], x[3], y[20], sigma,
                  deltay[20], delt, sig;

/* identify program to user */
    printf("%s\n\n", "Program E3LSQ demonstrates use of LSQNON.");
/* create simulated data */
    x[0] = 1.;
    x[1] = 1.2;
    x[2] = .4;
    nseed1 = 15;
    nseed2 = 215;
    rne2in_(&nseed1, &nseed2);
    rnstnr_(deltay, &c__20);
    rnecuy_(y, &c__20);
    delt = .09523809523809523;
    sigma = .1;
    printf("%s\n", "   T         Y       DELTAY");
    for (i = 1; i <= 20; ++i) {
	t[i - 1] = (double) i * delt;
	sig = sigma * (.5 + y[i - 1]);
	y[i - 1] = lsqgss_(x, &c__3, &t[i - 1]) + deltay[i - 1] * sig;
	deltay[i - 1] = sig;
        printf("%10.5lf%10.5lf%10.5lf\n", t[i-1], y[i-1], deltay[i-1]);
    }
    printf("\n");
/* loop over different numbers of unfixed variables */
    for (nloop = 3; nloop >= 0; --nloop) {
        nred = nloop;
/* set first approximation of unknowns and perform fit with LSQNON */
	if (nred == 3) {
	    x[0] = .8;
	    x[1] = .8;
	    x[2] = .8;
	    list[0] = 1;
	    list[1] = 1;
	    list[2] = 1;
	} else if (nred == 2) {
	    x[0] = .8;
	    x[1] = 1.2;
	    x[2] = .8;
	    list[0] = 1;
	    list[1] = 0;
	    list[2] = 1;
	} else if (nred == 1) {
	    x[0] = .8;
	    x[1] = 1.2;
	    x[2] = .4;
	    list[0] = 1;
	    list[1] = 0;
	    list[2] = 0;
	} else if (nred == 0) {
	    x[0] = 1.;
	    x[1] = 1.2;
	    x[2] = .4;
	    list[0] = 0;
	    list[1] = 0;
	    list[2] = 0;
	}
        printf("%s%2li%s%2li%s%", "NR =", nr, " , NRED = ", nred,
               " , LIST = ");
        for (k = 1; k <= nr; ++k)
           printf("%2li", list[k-1]);
        printf("%s", ", first appr. X = ");
        for (k = 1; k <= nr; ++k)
           printf("%5.1lf", x[k-1]);
        printf("\n");
	nstep = 100;
	lsqnon_(lsqgss_, t, y, deltay, &c__20, &c__3, &nred, list, x, 
	       cx, &r, a, gx, &nstep);
/* output results */
        printf("%s\n", "Result of fit with LSQNON:");
        printf("%s%10.5lf%s%5li\n", "Fit to Gaussian, M = ", r,
               " , NSTEP = ", nstep);
        printf("%s", "X = ");
        for (k = 1; k <= nr; ++k)
            printf("%10.5lf", x[k-1]);
        printf("\n");
	if (nred > 0) {
            printf("%s\n", "Covariance matrix CX =");
	    mtxwrt_(cx, &nred, &nred);
            printf("\n");
	}
    }
    return 0;
} /* main */

