#include <stdio.h>
#include <math.h>
#include "datsrc.h"

extern double CBFUNC funct(double const P_T *x, integer const P_T *n);

/* Common Block Declarations */

struct funtyp {
    integer itype;
} LIBDATA funtyp;

/* Table of constant values */

static integer c__3 = 3;
static double c_d0 = 0.;

main(void)
{
    /* Local variables */
    static integer i, k, list[3], method, npt, nred, nstep;
    static double f, fmin, x[3], scrat[12] /* was [3][4] */, xin[3];
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S1MIN determines first approximation");
    printf("%s\n\n", "of minimum position by Monte Carlo trials.");
/* ask for minimization method */
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - minimization by MINSIM");
    printf("%s\n", "2 - minimization by MINPOW");
    printf("%s\n", "3 - minimization by MINCJG");
    printf("%s\n", "4 - minimization by MINQDR");
    printf("%s\n", "5 - minimization by MINMAR");
    printf("%s", "> ");
    scanf("%li", &method);
/* ask for number of trial points for first approximation */
    printf("%s\n", "Enter number NPT of trial points");
    printf("%s\n", "for first approximation:");
    printf("%s", "> ");
    scanf("%li", &npt);
/* look for point with lowest function value among trial points */
    fmin = 1e30;
    i__1 = npt;
    for (i = 1; i <= i__1; ++i) {
	rnecuy_(x, &c__3);
	for (k = 1; k <= 3; ++k) {
	    x[k - 1] = x[k - 1] * 20. - 10.;
	}
	f = funct(x, &c__3);
	if (f < fmin) {
	    fmin = f;
	    mtxcpv_(x, xin, &c__3);
	}
    }
/* set initial values of variables and set LIST */
    mtxcpv_(xin, x, &c__3);
    nred = 3;
    nstep = 1000;
    fmin = 0.;
    for (i = 1; i <= 3; ++i) {
	if (i <= nred) {
	    list[i - 1] = 1;
	} else {
	    list[i - 1] = 0;
	}
    }
/* write out initial conditions */
    printf("%s%2li%s%2li%s%2li%2li%2li\n", "N = ", c__3, " , NRED = ", nred,
           " , LIST = ", list[0], list[1], list[2]);
    printf("%s%10.5lf%10.5lf%10.5lf\n", "first approx. at X = ",
           x[0], x[1], x[2]);
/* perform minimization */
    if (method == 1) {
        printf("%s\n", "minimization by MINSIM");
	minsim_(x, &c__3, &nred, list, funct, &fmin, &c_d0, &nstep, scrat);
    } else if (method == 2) {
        printf("%s\n", "minimization by MINPOW");
	minpow_(x, &c__3, &nred, list, funct, &fmin, &c_d0, &nstep, scrat);
    } else if (method == 3) {
        printf("%s\n", "minimization by MINCJG");
	mincjg_(x, &c__3, &nred, list, funct, &fmin, &c_d0, &nstep);
    } else if (method == 4) {
        printf("%s\n", "minimization by MINQDR");
	minqdr_(x, &c__3, &nred, list, funct, &fmin, &c_d0, &nstep, scrat);
    } else if (method == 5) {
        printf("%s\n", "minimization by MINMAR");
	minmar_(x, &c__3, &nred, list, funct, &fmin, &c_d0, &nstep, scrat);
    }
/* write out results */
    printf("%s%5li\n", "NSTEP = ", nstep);
    printf("%s%10.5lf%10.5lf%10.5lf\n", "minimum found at X = ",
           x[0], x[1], x[2]);
    printf("%s%15.5lG\n", "minimum of function is FMIN = ", fmin);
    return 0;
} /* main */

/* ---------------------------------------------------------------- */
double CBFUNC funct(double const P_T *x, integer const P_T *n)
{
    /* Initialized data */

    static double a[3] = { 3.,3.,3. };

    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

    /* Local variables */
    static double r2, ra2;

    /* Parameter adjustments */
    --x;

    /* Function Body */
/* function to be minimized */
/* Computing 2nd power */
    d__1 = x[1];
/* Computing 2nd power */
    d__2 = x[2];
/* Computing 2nd power */
    d__3 = x[3];
    r2 = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
    if (r2 > 30.) {
	ret_val = 0.;
    } else {
/* Computing 2nd power */
	d__1 = x[1] - a[0];
/* Computing 2nd power */
	d__2 = x[2] - a[1];
/* Computing 2nd power */
	d__3 = x[3] - a[2];
	ra2 = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
	ret_val = -exp(-r2) - exp(-ra2) * 10.;
    }
    return ret_val;
} /* funct */

