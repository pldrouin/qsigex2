#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"

main(void)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i, n, iexp, nexp, nf;
    static double dels, q, r[1000], s, t, sigma, xmean, delxm, x0, dels2, c,
                  confid, al0, ssq;
    static char text[15];
/* identify program to user */
    printf("%s\n", "Program E2TEST simulates Gaussian sample");
    printf("%s\n\n", "and performs t-test on mean.");
/* ask for input */
    printf("%s\n", "Enter number NEXP of experiments (>0):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter number N of elements in sample (>0):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter mean X0 for sample:");
    printf("%s", "> ");
    scanf("%lf", &x0);
    printf("%s\n", "Enter standard deviation SIGMA for sample (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter hypothesis AL0 for mean:");
    printf("%s", "> ");
    scanf("%lf", &al0);
    printf("%s\n", "Enter confidence limit CONFID (0.<CONFID<1.):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    c = (confid + 1.) * .5;
    nf = n - 1;
    q = sqstud_(&c, &nf);
/* write heading for table */
    printf("%s\n", "IEXP    N       XMEAN       t          Q");
/* loop over all experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
/* simulate sample R */
	rnstnr_(r, &n);
	i__2 = n;
	for (i = 1; i <= i__2; ++i) {
	    r[i - 1] = x0 + r[i - 1] * sigma;
	}
/* compute sample mean and variance and t */
	smmnvr_(r, &n, &xmean, &delxm, &ssq, &dels2, &s, &dels);
	t = (xmean - al0) * sqrt((double) n / ssq);
	if (abs(t) > q) {
	    strcpy(text, " t-test failed");
	} else {
	    strcpy(text, "              ");
	}
/* output */
        printf("%5li%5li%12.5lf%12.5lf%12.5lf%s\n",
               iexp, n, xmean, t, q, text);
    }
    return 0;
} /* main */

