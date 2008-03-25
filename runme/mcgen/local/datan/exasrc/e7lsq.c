#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__8 = 8;

main(void)
{
    /* Initialized data */ /* Local variables */

    static integer i, m, n, nstep, nr, nred, list[2];
    static double t[4] = { .2,.5,.8,1.3 }, s[4] = { .15,.4,.7,.8 },
                  dt[4] = { .1,.1,.1,.05 }, ds[4] = { .05,.1,.1,.1 },
                  rho[4] = { 0.,0.,.5,0. }, e[40] /* was [4][10] */,
                  f[100] /* was [10][10] */, r, x[2], y[8],
                  a2[100] /* was [10][10] */, cx[4] /* was [2][2] */,
                  cy[64] /* was [8][8] */, gy[64] /* was [8][8] */;

    /* System generated locals */
    double d__1;

/* identify program to user */
    printf("%s\n\n", "Program E7LSQ demonstrates use of LSQGEN.");
/* write table of data */
    printf("%s\n", "    T         S         DT        DS        RHO");
    for (i = 1; i <= 4; ++i) {
        printf("%10.5lf%10.5lf%10.5lf%10.5lf%10.5lf\n", 
               t[i-1], s[i-1], dt[i-1], ds[i-1], rho[i-1]);
    }
    printf("\n");
/* set up data for input to LSQGEN */
    n = 8;
    m = 4;
    nr = 2;
    for (nred = 2; nred >= 0; --nred) {
	mtxunt_(cy, &c__8);
	for (i = 1; i <= 4; ++i) {
	    y[(i - 1) * 2] = t[i - 1];
	    y[(i - 1 << 1) + 1] = s[i - 1];
/* Computing 2nd power */
	    d__1 = dt[i - 1];
	    cy[(i - 1 << 1) + 1 + ((i - 1 << 1) + 1 << 3) - 9] = d__1 * d__1;
/* Computing 2nd power */
	    d__1 = ds[i - 1];
	    cy[(i - 1 << 1) + 2 + ((i - 1 << 1) + 2 << 3) - 9] = d__1 * d__1;
	    cy[(i - 1 << 1) + 1 + ((i - 1 << 1) + 2 << 3) - 9] = rho[i - 1] * 
		    ds[i - 1] * dt[i - 1];
	    cy[(i - 1 << 1) + 2 + ((i - 1 << 1) + 1 << 3) - 9] = rho[i - 1] * 
		    ds[i - 1] * dt[i - 1];
	}
/* determine first approximation */
	if (nred == 2) {
	    list[0] = 1;
	    list[1] = 1;
	    x[1] = (s[3] - s[0]) / (t[3] - t[0]);
	    x[0] = s[0] - x[1] * t[0];
	} else if (nred == 1) {
	    list[0] = 0;
	    list[1] = 1;
	    x[0] = .2;
	    x[1] = (s[3] - s[0]) / (t[3] - t[0]);
	} else if (nred == 0) {
	    list[0] = 0;
	    list[1] = 0;
	    x[0] = 0.;
	    x[1] = .5;
	}
/* header for output of results */
        printf("%s\n", "performing fit with LSQGEN");
        printf("%s%2li%s%2li%s%2li%s%2li%2li\n", "N = ", n, ", NR = ", nr,
               ", NRED = ", nred, ", LIST = ", list[0], list[1]);
        printf("%s%10.5lf%10.5lf\n", "first approx.: X = ", x[0], x[1]);
	nstep = 100;
	lsqgen_(y, cy, gy, f, e, &m, &n, &nr, &nred, list, x, cx, &r,
               a2, &nstep);
/* output of results */
        printf("%s%6.2lf%s%3li\n%s%10.5lf%10.5lf\n", "result of fit: R = ", r,
               ", NSTEP =", nstep, "X =", x[0], x[1]);
	if (nred > 0) {
            printf("%s\n", "covariance matrix CX = ");
	    mtxwrt_(cx, &nred, &nred);
	}
        printf("\n");
    }
    return 0;
} /* main */

