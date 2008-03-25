#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "datsrc.h"

main(void)
{
    /* Local variables */
    static integer i, n, iexp, nexp, npt;
    static double xbar[2], spsq[2], a[2], c[4] /* was [2][2] */,
                  r, x[2], sigma[2], dplus[4] /* was [2][2] */, 
                  rho, sprime[2] /*, xsampl[20000]*/ /* was [10000][2] */;
#if defined(_MSC_VER)
    static double __huge xsampl[20000];
#elif defined(__TURBOC__)
    static double __huge * xsampl;
#else
    static double xsampl[20000];
#endif
    /* System generated locals */
    integer i__1, i__2;
    double d__1, d__2;
/* identify program to user */
    printf("%s\n", "Program E2ML estimates parameters");
    printf("%s\n\n", "of bivariate Gaussian.");
/* ask for input */
    printf("%s\n", "Enter number NEXP of simulated experiments (>0):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter number NPT of points in each experiment"
                   " (2<NPT<10001):");
    printf("%s", "> ");
    scanf("%li", &npt);
#if defined(_MSC_VER)
/*
    xsampl = (double __huge *) _halloc(2L * npt, sizeof(double));
*/
#elif defined(__TURBOC__)
    xsampl = (double __huge *) farmalloc(2L * npt * sizeof(double));
#endif
    if (!xsampl) {
       MEMERROR("e2ml: xsampl");
       return(0);
    }
    printf("%s\n", "Enter a1:");
    printf("%s", "> ");
    scanf("%lf", &a[0]);
    printf("%s\n", "Enter a2:");
    printf("%s", "> ");
    scanf("%lf", &a[1]);
    printf("%s\n", "Enter sigma1 (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma[0]);
    printf("%s\n", "Enter sigma2 (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma[1]);
    printf("%s\n", "Enter rho (-1.<rho<1.):");
    printf("%s", "> ");
    scanf("%lf", &rho);
/* N=2 defines number of variables */
/* in multivariate normal distribution */
    n = 2;
/* set up covariance matrix */
/* Computing 2nd power */
    d__1 = sigma[0];
    c[0] = d__1 * d__1;
/* Computing 2nd power */
    d__1 = sigma[1];
    c[3] = d__1 * d__1;
    c[2] = rho * sigma[0] * sigma[1];
    c[1] = c[2];
/* prepare for generation of random numbers */
    rnmnpr_(c, dplus, &n);
/* write header for output */
    printf("%s\n", " XBAR(1)   XBAR(2)   SPRIME(1) SPRIME(2)     R");
/* loop over all simulation experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
/* generate sample of 2-vectors from bivariate normal */
/* and compute sample means */
	xbar[0] = 0.;
	xbar[1] = 0.;
	i__2 = npt;
	for (i = 1; i <= i__2; ++i) {
	    rnmngn_(dplus, a, x, &n);
	    xsampl[i - 1] = x[0];
	    xsampl[i + npt - 1] = x[1];
	    xbar[0] += x[0];
	    xbar[1] += x[1];
	}
	xbar[0] /= (double) npt;
	xbar[1] /= (double) npt;
/* compute sample variances and correlation coefficient */
	spsq[0] = 0.;
	spsq[1] = 0.;
	r = 0.;
	i__2 = npt;
	for (i = 1; i <= i__2; ++i) {
/* Computing 2nd power */
	    d__1 = xsampl[i - 1] - xbar[0];
	    spsq[0] += d__1 * d__1;
/* Computing 2nd power */
	    d__2 = xsampl[i + npt - 1] - xbar[1];
	    spsq[1] += d__2 * d__2;
	    r += d__1 * d__2;
	}
	spsq[0] /= (double) npt;
	spsq[1] /= (double) npt;
	sprime[0] = sqrt(spsq[0]);
	sprime[1] = sqrt(spsq[1]);
	r /= (double) npt * sprime[0] * sprime[1];
        printf("%10.5lf%10.5lf%10.5lf%10.5lf%10.5lf\n",
               xbar[0], xbar[1], sprime[0], sprime[1], r);
    }
#if defined(_MSC_VER)
/*
    _hfree((void __huge *) xsampl);
*/
#elif defined(__TURBOC__)
    farfree((void far *) xsampl);
#endif
    return 0;
} /* main */

