#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__10 = 10;

main(void)
{
    /* Initialized data */

    static integer i;
    static integer nr;
    static double t[10] = { -.9,-.7,-.5,-.3,-.1,.1,.3,.5,.7,.9 },
                  y[10] = { -1.6,-1.5,-.8,-.9,-.4,0.,.5,1.,1.2,1.4 },
                  deltay[10] = { .2,.3,.1,.2,.1,.2,.2,.3,.2,.15 },
                  a[10] /* was [10][1] */, c[10], scrat[1] /* was [1][1] */,
                  cx[1] /* was [1][1] */, r, x[1];
    static logical ok;

/* identify program to user */
    printf("%s\n\n", "Program E2LSQ demonstrates use of LSQLIN.");
/* set up matrix A and vector C */
    for (i = 1; i <= 10; ++i) {
	a[i - 1] = -t[i - 1];
	c[i - 1] = -y[i - 1];
    }
/* perform fit to proportionality */
    nr = 1;
    lsqlin_(t, c, deltay, &c__10, &nr, x, cx, &r, a, scrat, &ok);
/* output results */
    printf("%s\n", "   T  ,      Y       DELTAY");
    for (i = 1; i <= 10; ++i) {
        printf("%10.5lf%10.5lf%10.5lf\n", t[i-1], y[i-1], deltay[i-1]);
    }
    printf("\n%s%10.5lf\n", "Fit to proportionality, M = ", r);
    printf("%s%10.5lf\n", "X = ", x[0]);
    printf("%s\n", "Covariance  CX =");
    mtxwrt_(cx, &nr, &nr);
    return 0;
} /* main */

