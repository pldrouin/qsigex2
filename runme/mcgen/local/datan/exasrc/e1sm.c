#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__100 = 100;

main(void)
{
    static double dels, dels2;
    static integer i;
    static double s, xmean, delxm, s2, sample[100];

/* identify program to user */
    printf("%s\n", "Program E1SM demonstrates the use of SMMNVR");
    printf("%s\n", "on 10 samples of 100 elements each taken");
    printf("%s\n\n", "from the standardized normal distribution.");
/* write table caption */
    printf("%s\n",
           "   XMEAN     DELXM     S2        DELS2     S          DELS");
/* perform loop over 10 simulated samples of 100 elements each */
    for (i = 1; i <= 10; ++i) {
	rnstnr_(sample, &c__100);
	smmnvr_(sample, &c__100, &xmean, &delxm, &s2, &dels2, &s, &dels);
        printf("%10.5lf%10.5lf%10.5lf%10.5lf%10.5lf%10.5lf\n",
               xmean,delxm,s2,dels2,s,dels);
    }
    return 0;
} /* main */

