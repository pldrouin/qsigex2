#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__4 = 4, c__6 = 6;

main(void)
{
    static integer i, j, k, ndf[6], ntype, iseed1, iseed2;
    static double ab, ai, bj, xb, buffer[6], abij, xbij[16] /* was [4][4] */,
                  a, b, f[4], q[6], s[6], alpha[4], x[96] /* was [4][4][6] */,
                  sigma, xbi[4], xbj[4], amu;
/* identify program to user */
    printf("%s\n", "Program S1AV generates data of crossed classification");
    printf("%s\n\n", "and performs analysis of variance on them.");
/* ask for input */
    printf("%s\n", "Enter A:");
    printf("%s", "> ");
    scanf("%lf", &a);
    printf("%s\n", "Enter B:");
    printf("%s", "> ");
    scanf("%lf", &b);
    printf("%s\n", "Enter AB:");
    printf("%s", "> ");
    scanf("%lf", &ab);
    printf("%s\n", "Enter MU:");
    printf("%s", "> ");
    scanf("%lf", &amu);
    printf("%s\n", "Enter SIGMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter ISEED1 (>0):");
    printf("%s", "> ");
    scanf("%li", &iseed1);
    printf("%s\n", "Enter ISEED2 (>0):");
    printf("%s", "> ");
    scanf("%li", &iseed2);
/* initialize random number generator */
    rne2in_(&iseed1, &iseed2);
/* generate data */
    for (i = 1; i <= 4; ++i) {
	ai = ((double) i - (double) (i + 1) * .5) * a;
	for (j = 1; j <= 4; ++j) {
	    bj = ((double) j - (double) (j + 1) * .5) * b;
	    rnstnr_(buffer, &c__6);
	    for (k = 1; k <= 6; ++k) {
		abij = ((double) i - (double) (i + 1) * .5 + j -
                        (double) (j + 1) * .5) * ab;
		x[i + ((j + (k << 2)) << 2) - 21] = amu + ai + bj + abij + 
			buffer[k - 1] * sigma;
	    }
	}
    }
    avtble_(x, &c__4, &c__4, &c__6, &xb, xbi, xbj, xbij, q, s, f, ndf, alpha);
    ntype = 0;
    avoutp_(x, &c__4, &c__4, &c__6, q, s, f, ndf, alpha, &ntype);
    return 0;
} /* main */

