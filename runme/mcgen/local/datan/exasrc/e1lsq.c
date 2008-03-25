#include <stdio.h>
#include <math.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__10 = 10;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer i, k, nr;
    static integer c__1 = 1;
    static double t[10] = { -.9,-.7,-.5,-.3,-.1,.1,.3,.5,.7,.9 },
                  y[10] = { 81.,50.,35.,27.,26.,60.,106.,189.,318.,520. },
                  a[40] /* was [10][4] */, scrat[16] /* was [4][4] */,
                  cx[16] /* was [4][4] */, r, x[4], deltay[10];
    static logical ok;

/* identify program to user */
    printf("%s\n\n", "Program E1LSQ demonstrates use of LSQPOL.");
/* list data */
    printf("%s\n", "   T  ,      Y       DELTAY");
    for (i = 1; i <= 10; ++i) {
        deltay[i - 1] = sqrt(y[i - 1]);
        printf("%10.5lf%10.5lf%10.5lf\n", t[i-1], y[i-1], deltay[i-1]);
    }
    printf("\n");
/* perform fits with polynomials of different degree */
    for (nr = 1; nr <= 4; ++nr) {
        lsqpol_(t, y, deltay, &c__10, &nr, x, cx, &r, a, scrat, &ok);
        printf("%s%1li%s%10.5lf\n", "Polynomial fit with NR = ", nr,
               ", M = ", r);
        printf("%s\n", "X = ");
        mtxwrt_(x, &c__1, &nr);
        printf("\n");
        printf("%s\n", "Covariance matrix CX =");
        mtxwrt_(cx, &nr, &nr);
        printf("\n");
    }
    return 0;
} /* main */

