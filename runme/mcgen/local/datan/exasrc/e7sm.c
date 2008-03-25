#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1;

main(void)
{
    /* Local variables */
    static integer nexp, i, j, k, n, kb, id, ks;
    static double pref, pback, p, rback, r, alref, psign, rsign, rsmin, rspls,
	          rsupr, alback, confid, alsign;
    static char *frm1000 = "%s%10.5lf%s%10.8lf%s%10.8lf\n";

/* identify program to user */
    printf("%s\n", "Program E7SM simulates experiments with few events");
    printf("%s\n\n", "and reference events.");
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
    printf("%s\n", "Enter Poisson parameter ALREF for reference evts (>0.):");
    printf("%s", "> ");
    scanf("%lf", &alref);
    printf("%s\n", "Enter confidence limit CONFID (0.<CONFID<1.):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    psign = alsign / (double) n;
    pback = alback / (double) n;
    pref = alref / (double) n;
    rsign = psign / pref;
    rback = pback / pref;
    p = psign + pback + pref;
/* write table header */
    printf("\n%5ld%s\n", nexp, " experiments will be simulated with");
    printf(frm1000, "ALSIGN = ", alsign, " i.e. PSIGN = ", psign,
           " and RSIGN = ", rsign);
    printf(frm1000, "ALBACK = ", alback, " i.e. PBACK = ", pback,
           " and RBACK = ", rback);
    printf("%s%10.5lf%s%10.8lf\n", "ALREF  = ", alref, " i.e. PREF  = ", pref);
    printf("%s%5.3lf\n", "Limits for RSIGN are obtained at a"
           " confidence level of ", confid);
    printf("\n%s\n", "    I   KS   KB    K   ID     RSMIN     RSPLS     RSUPR");
/* loop over simulated experiments */
    for (i = 1; i <= nexp; ++i) {
	ks = 0;
	kb = 0;
	id = 0;
	for (j = 1; j <= n; ++j) {
	    rnecuy_(&r, &c__1);
	    if (r < p) {
		if (r < psign) {
		    ++ks;
		} else if (r >= psign && r < psign + pback) {
		    ++kb;
		} else {
		    ++id;
		}
	    }
	}
	k = ks + kb;
/* compute confidence limits */
	smerqs_(&k, &id, &confid, &rback, &rsmin, &rspls, &rsupr);
/* output */
        printf("%5ld%5ld%5ld%5ld%5ld%10.5lf%10.5lf%10.5lf\n",
               i, ks, kb, k, id, rsmin, rspls, rsupr);
    }
    return 0;
} /* main */

