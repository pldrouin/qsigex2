#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__4 = 4, c__6 = 6;

main(void)
{
    static integer i, j, k, ntype, iseed1, iseed2, ndf[6];
    static double xbij[16] /* was [4][4] */, f[4], q[6], s[6], alpha[4],
                  x[96] /* was [4][4][6] */, sigma, xb, buffer[6],
                  xbi[4], xbj[4];

/* identify program to user */
    printf("%s\n", "program E2AV generates data and performs");
    printf("%s\n\n", "analysis of variance on them.");
/* ask for input */
    printf("%s\n", "Enter SIGMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter ISEED1 (>0):");
    printf("%s", "> ");
    scanf("%li", &iseed1);
    printf("%s\n", "Enter ISEED2 (>0):");
    printf("%s", "> ");
    scanf("%li", &iseed2);
    printf("%s\n", "Choose:");
    printf("%s\n", "0 - crossed classification");
    printf("%s\n", "1 - nested classification");
    printf("%s", "> ");
    scanf("%li", &ntype);
/* initialize random number generator */
    rne2in_(&iseed1, &iseed2);
/* generate data */
    for (i = 1; i <= 4; ++i) {
	for (j = 1; j <= 4; ++j) {
	    rnstnr_(buffer, &c__6);
	    for (k = 1; k <= 6; ++k) {
		x[i + (j + (k << 2) << 2) - 21] = buffer[k - 1] * sigma;
	    }
	}
    }
    avtble_(x, &c__4, &c__4, &c__6, &xb, xbi, xbj, xbij, q, s, f, ndf, alpha);
    avoutp_(x, &c__4, &c__4, &c__6, q, s, f, ndf, alpha, &ntype);
    return 0;
} /* main */

