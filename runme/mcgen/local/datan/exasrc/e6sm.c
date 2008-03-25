#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1;

main(void)
{
    /* Local variables */
    static integer nexp, i, j, k, n, kb, ks;
    static double pback, p, r, psign, alback, confid,
                  alsign, alsmin, alspls, alsupr;
    static char *frm1000 = "%s%10.5lf%s%10.8lf\n";

/* identify program to user */
    printf("%s\n\n", "Program E6SM simulates experiments with few events.");
/* ask for input */
    printf("%s\n", "Enter number NEXP of experiments (>0):");
    printf("%s", "> ");
    scanf("%ld", &nexp);
    printf("%s\n", "Enter number N of studied objects (>>0):");
    printf("%s", "> ");
    scanf("%ld", &n);
    printf("%s\n", "Enter Poisson parameter ALSIGN for signal events (>0.):");
    printf("%s", "> ");
    scanf("%lf", &alsign);
    printf("%s\n",
           "Enter Poisson parameter ALBACK for background evts (>=0.):");
    printf("%s", "> ");
    scanf("%lf", &alback);
    printf("%s\n", "Enter confidence limit CONFID (0.<CONFID<1.):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    psign = alsign / (double) n;
    pback = alback / (double) n;
    p = psign + pback;
/* write table header */
    printf("\n%5ld%s\n", nexp, " experiments will be simulated with");
    printf(frm1000, "ALSIGN = ", alsign," i.e. PSIGN = ", psign);
    printf(frm1000, "ALBACK = ", alback," i.e. PBACK = ", pback);
    printf("%s%5.3lf\n", "Limits for ALSIGN are obtained at a"
           " confidence level of ", confid);
    printf("\n%s\n", "    I   KS   KB    K    ALSMIN    ALSPLS    ALSUPR");
/* loop over simulated experiments */
    for (i = 1; i <= nexp; ++i) {
	k = 0;
	ks = 0;
	kb = 0;
	for (j = 1; j <= n; ++j) {
	    rnecuy_(&r, &c__1);
	    if (r < p) {
		++k;
		if (r < psign) {
		    ++ks;
		} else {
		    ++kb;
		}
	    }
	}
/* compute confidence limits */
	smerss_(&k, &confid, &alback, &alsmin, &alspls, &alsupr);
/* output */
        printf("%5ld%5ld%5ld%5ld%10.5lf%10.5lf%10.5lf\n",
               i, ks, kb, k, alsmin, alspls, alsupr);
    }
    return 0;
} /* main */

