#include <stdio.h>
#include <math.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__9 = 9, c__50 = 50;

main(void)
{
    static integer i, nr = 9, nstep, nseed1, nseed2, nred, list[9];
    static double a[450] /* was [50][9] */, cx[81] /* was [9][9] */,
                  gx[81] /* was [9][9] */, deltay[50], r, t[50], x[9], y[50],
                  sigma, delt, sig;

/* identify program to user */
    printf("%s\n\n", "Program E4LSQ demonstrates use of LSQMAR.");
/* create simulated data */
    x[0] = 5.;
    x[1] = -.005;
    x[2] = -.001;
    x[3] = 10.;
    x[4] = 10.;
    x[5] = 2.;
    x[6] = 10.;
    x[7] = 30.;
    x[8] = 2.;
    nseed1 = 15;
    nseed2 = 135;
    rne2in_(&nseed1, &nseed2);
    rnstnr_(deltay, &c__50);
    rnecuy_(y, &c__50);
    delt = 1.;
    sigma = .4;
    printf("%s\n", "   T         Y       DELTAY");
    for (i = 1; i <= 50; ++i) {
	t[i - 1] = (double) i * delt;
	sig = sigma * (.5 + y[i - 1]);
	y[i - 1] = lsqp2g_(x, &c__9, &t[i - 1]) + deltay[i - 1] * sig;
	deltay[i - 1] = sig;
        printf("%10.5lf%10.5lf%10.5lf\n", t[i-1], y[i-1], deltay[i-1]);
    }
    printf("\n");
/* set first approximation of unknowns and perform fit with LSQMAR */
    nred = 9;
    x[0] = 0.;
    x[1] = 0.;
    x[2] = 0.;
    x[3] = 8.;
    x[4] = 12.;
    x[5] = 3.;
    x[6] = 12.;
    x[7] = 28.;
    x[8] = 1.5;
    for (i = 1; i <= 9; ++i) {
	list[i - 1] = 1;
    }
    printf("%s%2li%s%2li%s", "NR =", nr, " , NRED = ", nred, " , LIST = ");
    for (i = 1; i <= 9; ++i)
        printf("%2li", list[i-1]);
    printf("%s\n%s", ", first appr.:", "X = ");
    for (i = 1; i <= 9; ++i)
        printf("%8.3lf", x[i-1]);
    printf("\n");
    nstep = 100;
    lsqmar_(lsqp2g_, t, y, deltay, &c__50, &c__9, &nred, list, x, cx, &r, a,
           gx, &nstep);
/* output results */
    printf("%s\n", "Result of fit with LSQMAR:");
    printf("%s%10.5lf%s%5li\n", "Fit to Gaussian, M = ", r,
           ", NSTEP = ", nstep);
    printf("%s", "X = ");
    for (i = 1; i <= 9; ++i)
        printf("%8.3lf", x[i-1]);
    printf("\n");
    return 0;
} /* main */

/* ---------------------------------------------------------- */
void LIBFUNC auxdri_(double (CBFUNC P_T *dummy)(double const P_T *,
                    integer const P_T *, double const P_T *), double P_T *x,
                    double const P_T *t, integer const P_T *n,
                    integer const P_T *nr, integer const P_T *nred,
                    integer const P_T *list, double P_T *a, logical P_T *ok)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;
    double d__1, d__2, d__3;

    /* Local variables */
    static integer i, j, ix;
    static double g1, g2, arg1, arg2;

/* analytical derivatives of 2 Gaussians on polynomial background */
    /* Parameter adjustments */
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --list;
    --t;
    --x;

    /* Function Body */
    *ok = TRUE_;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
	d__1 = t[i] - x[5];
/* Computing 2nd power */
	d__2 = x[6];
	arg1 = d__1 * d__1 / (d__2 * d__2 * 2.);
	if (arg1 > 100.) {
	    g1 = 0.;
	} else {
	    g1 = exp(-arg1);
	}
/* Computing 2nd power */
	d__1 = t[i] - x[8];
/* Computing 2nd power */
	d__2 = x[9];
	arg2 = d__1 * d__1 / (d__2 * d__2 * 2.);
	if (arg2 > 100.) {
	    g2 = 0.;
	} else {
	    g2 = exp(-arg2);
	}
	ix = 0;
	i__2 = *nr;
	for (j = 1; j <= i__2; ++j) {
	    if (list[j] != 0) {
		++ix;
		if (j == 1) {
		    a[i + ix * a_dim1] = 1.;
		} else if (j == 2) {
		    a[i + ix * a_dim1] = t[i];
		} else if (j == 3) {
/* Computing 2nd power */
		    d__1 = t[i];
		    a[i + ix * a_dim1] = d__1 * d__1;
		} else if (j == 4) {
		    a[i + ix * a_dim1] = g1;
		} else if (j == 5) {
/* Computing 2nd power */
		    d__1 = x[6];
		    a[i + ix * a_dim1] = x[4] * g1 * (t[i] - x[5]) /
                                         (d__1 * d__1);
		} else if (j == 6) {
/* Computing 2nd power */
		    d__1 = t[i] - x[5];
/* Computing 3rd power */
		    d__2 = x[6], d__3 = d__2;
		    a[i + ix * a_dim1] = x[4] * g1 * (d__1 * d__1) /
                                         (d__3 * (d__2 * d__2));
		} else if (j == 7) {
		    a[i + ix * a_dim1] = g2;
		} else if (j == 8) {
/* Computing 2nd power */
		    d__1 = x[9];
		    a[i + ix * a_dim1] = x[7] * g2 * (t[i] - x[8]) /
                                         (d__1 * d__1);
		} else if (j == 9) {
/* Computing 2nd power */
		    d__1 = t[i] - x[8];
/* Computing 3rd power */
		    d__2 = x[9], d__3 = d__2;
		    a[i + ix * a_dim1] = x[7] * g2 * (d__1 * d__1) /
                                         (d__3 * (d__2 * d__2));
		}
	    }
	}
    }
} /* auxdri_ */

