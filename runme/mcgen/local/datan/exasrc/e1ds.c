#include <stdio.h>
#include "datsrc.h"

main(void)
{
    /* Local variables */
    static integer iexp, nfly, nexp, na, it, nt, nnfly;
    static double p, r[1000], ha;
    /* System generated locals */
    integer i__1, i__2;
/* simulates Example 5.1 */
/* identify program to user */
    printf("%s\n", "Program E1DS simulates empirical frequency");
    printf("%s\n\n", "and demonstrates statistical fluctuations.");
/* ask for parameters */
    printf("%s\n", "Enter number of flies NFLY (>0):");
    printf("%s", "> ");
    scanf("%li", &nfly);
    printf("%s\n", "Enter number of experiments NEXP (>0):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter probability P(A), (0.<P(A)<1.):");
    printf("%s", "> ");
    scanf("%lf", &p);
/* loop over all experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
	nnfly = nfly;
	na = 0;
/* loop over all flies */
L10:
	nt = min(nnfly,1000);
	rnecuy_(r, &nt);
	i__2 = nt;
	for (it = 1; it <= i__2; ++it) {
	    if (r[it - 1] < p) {
		++na;
	    }
	}
	nnfly -= nt;
	if (nnfly > 0) {
	    goto L10;
	} else {
	    ha = (double) na / (double) nfly;
            printf("%s%3li%s%5li%s%10.8lf\n",
                   " IEXP = ", iexp, " ,  NA = ", na, " ,  HA = ", ha);
	}
    }
    return 0;
} /* main */

