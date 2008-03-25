#include <stdio.h>
#include "datsrc.h"

main(void)
{
    static integer k;
    static double alback, confid, alsmin, alspls, alsupr;

/* identify program to user */
    printf("%s\n", "Program E4SM demonstrates use of SMERSS");
    printf("%s\n", "by computing error of Poisson parameter");
    printf("%s\n\n", "derived from a small sample with background.");
/* ask for input */
    printf("%s\n", "Enter number K of observed events (>=0):");
    printf("%s", "> ");
    scanf("%ld", &k);
    printf("%s\n", "Enter confidence limit CONFID (0.<CONFID<1.):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    printf("%s\n", "Enter Poisson Parameter ALBACK of background (>=0.):");
    printf("%s", "> ");
    scanf("%lf", &alback);
    smerss_(&k, &confid, &alback, &alsmin, &alspls, &alsupr);
    printf("%s%lf\n", "ALSMIN = ", alsmin);
    printf("%s%lf\n", "ALSPLS = ", alspls);
    printf("%s%lf\n", "ALSUPR = ", alsupr);
    return 0;
} /* main */

