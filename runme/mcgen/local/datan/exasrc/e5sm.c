#include <stdio.h>
#include "datsrc.h"

main(void)
{
    static integer id, k;
    static double rback, rsmin, rspls, rsupr, confid;

/* identify program to user */
    printf("%s\n", "Program E5SM demonstrates use of SMERQS");
    printf("%s\n", "by computing errors of ratio");
    printf("%s\n", "of signal events and reference events");
    printf("%s\n\n", "in the presence of background.");
/* ask for input */
    printf("%s\n", "Enter number K of observed events (>0):");
    printf("%s", "> ");
    scanf("%ld", &k);
    printf("%s\n", "Enter number ID of observed reference events (>0):");
    printf("%s", "> ");
    scanf("%ld", &id);
    printf("%s\n", "Enter confidence limit CONFID (0.<CONFID<1.):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    printf("%s\n", "Enter ratio RBACK (>=0.):");
    printf("%s", "> ");
    scanf("%lf", &rback);
    smerqs_(&k, &id, &confid, &rback, &rsmin, &rspls, &rsupr);
    printf("%s%lf\n", "RSMIN = ", rsmin);
    printf("%s%lf\n", "RSPLS = ", rspls);
    printf("%s%lf\n", "RSUPR = ", rsupr);
    return 0;
} /* main */

