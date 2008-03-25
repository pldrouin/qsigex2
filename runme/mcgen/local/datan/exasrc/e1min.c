#include <stdio.h>
#include <math.h>
#include "datsrc.h"

extern double LIBFUNC funct(double const P_T *x, integer const P_T *n);

/* Common Block Declarations */

struct funtyp {
    integer itype;
} LIBDATA funtyp;

/* Table of constant values */

static integer c__3 = 3;
static double c_d0 = 0.;

main(void)
{
    static integer nvar, list[3], i, nred, nstep, method;
    static double fmin, x[3], scrat[12] /* was [3][4] */, xin[3];
/* identify program to user */
    printf("%s\n", "Program E1MIN demonstrates use of");
    printf("%s\n\n", "MINSIM,MINPOW,MINCJG,MINQDR,MINMAR.");
/* ask for minimization method */
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - minimization by MINSIM");
    printf("%s\n", "2 - minimization by MINPOW");
    printf("%s\n", "3 - minimization by MINCJG");
    printf("%s\n", "4 - minimization by MINQDR");
    printf("%s\n", "5 - minimization by MINMAR");
    printf("%s", "> ");
    scanf("%li", &method);
/* ask for type of function */
    printf("%s\n", "Choose function to be minimized:");
    printf("%s\n", "1 - f=r**2=x1**2+x2**2+x3**2");
    printf("%s\n", "2 - f=r**10");
    printf("%s\n", "3 - f=r");
    printf("%s\n", "4 - f=-exp(-r**2)");
    printf("%s\n", "5 - f=r**6-2*r**4+r**2");
    printf("%s\n", "6 - f=r**2*exp(-r**2)");
    printf("%s\n", "7 - f=-exp(-r**2)-10*exp(-ra**2)");
    printf("%s\n", "      ra**2=(x1-3)**2+(x2-3)**2+(x3-3)**2");
    printf("%s", "> ");
    scanf("%li", &funtyp.itype);
/* ask for initial values */
    for (i = 1; i <= 3; ++i) {
        printf("%s%2li%s\n", "Enter initial value of X(", i, "):");
        printf("%s", "> ");
        scanf("%lf", &xin[i-1]);
    }
/* loop over number of unfixed variables */
    for (nvar = 3; nvar >= 0; --nvar) {
/* set initial values of variables and set LIST */
	mtxcpv_(xin, x, &c__3);
	nred = nvar;
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
        printf(" N = %2li , NRED = %2li , LIST = %2li%2li%2li\n",
               c__3, nred, list[0], list[1], list[2]);
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
    }
    return 0;
} /* main */

/* ---------------------------------------------------------------- */
double LIBFUNC funct(double const P_T *x, integer const P_T *n)
{
    /* Initialized data */ /* Local variables */
    static double a[3] = { 3.,3.,3. }, r2, ra2;

    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

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
    if (funtyp.itype == 1) {
	ret_val = r2;
    } else if (funtyp.itype == 2) {
/* Computing 5th power */
	d__1 = r2, d__2 = d__1, d__1 *= d__1;
	ret_val = d__2 * (d__1 * d__1);
    } else if (funtyp.itype == 3) {
	ret_val = sqrt((abs(r2)));
    } else if (funtyp.itype == 4) {
	if (r2 > 30.) {
	    ret_val = 0.;
	} else {
	    ret_val = -exp(-r2);
	}
    } else if (funtyp.itype == 5) {
/* Computing 3rd power */
	d__1 = r2, d__2 = d__1;
/* Computing 2nd power */
	d__3 = r2;
	ret_val = d__2 * (d__1 * d__1) - d__3 * d__3 * 2. + r2;
    } else if (funtyp.itype == 6) {
	if (r2 > 30.) {
	    ret_val = 0.;
	} else {
	    ret_val = r2 * exp(-r2);
	}
    } else if (funtyp.itype == 7) {
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
    }
    return ret_val;
} /* funct */

