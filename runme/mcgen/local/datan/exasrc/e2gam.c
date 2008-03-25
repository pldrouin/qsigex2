#include <stdio.h>
#include "datsrc.h"

main(void)
{
    /* Local variables */
    static integer i, ires[12], k, n, nx;
/* identify program to user */
    printf("%s\n", "Program E2GAM demonstrates use of GBINCO");
    printf("%s\n", "and computes and prints ");
    printf("%s\n\n", "top of Pascal""s triangle.");
/* loop over N */
    for (n = 0; n <= 11; ++n) {
/* loop over K */
	for (k = 0; k <= n; ++k) {
/* compute binomial coefficient */
	    ires[k] = (integer) gbinco_(&n, &k);
	}
/* compute format */
	nx = (11 - n) * 3 + 1;
        for (i = 1; i <= nx; ++i)
            printf(" ");
/* write one line of triangle */
        for (i = 1; i <= n+1; ++i)
            printf("%6li", ires[i-1]);
        printf("\n");
    }
    return 0;
} /* main */

