#include <stdio.h>
#include "datsrc.h"

main(void)
{
    /* Initialized data */
    static integer ntype, ni, nj, nk, ndf[6];
    static double x[80] /* was [2][4][10] */ = { 34.,28.,54.,23.,44.,
	    42.,51.,43.,62.,31.,61.,32.,59.,25.,66.,24.,52.,26.,52.,26.,40.,
	    32.,57.,23.,52.,41.,46.,48.,61.,45.,70.,38.,67.,27.,59.,26.,63.,
	    31.,50.,27.,38.,34.,40.,29.,53.,34.,51.,36.,54.,41.,64.,32.,58.,
	    27.,67.,32.,60.,25.,44.,27.,36.,27.,43.,30.,51.,39.,49.,43.,60.,
	    37.,68.,34.,66.,28.,58.,30.,59.,26.,52.,30. },
            xbij[20] /* was [2][10] */, f[4], q[6], s[6], alpha[4],
            xb, xbi[2], xbj[10];

/* identify program to user */
    printf("%s\n", "Program E1AV demonstrates use of");
    printf("%s\n\n", "AVTBLE and AVOUTP.");
    ni = 2;
    nj = 10;
    nk = 4;
    ntype = 0;
    avtble_(x, &ni, &nj, &nk, &xb, xbi, xbj, xbij, q, s, f, ndf, alpha);
    avoutp_(x, &ni, &nj, &nk, q, s, f, ndf, alpha, &ntype);
    return 0;
} /* main */

