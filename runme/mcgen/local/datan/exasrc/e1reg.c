#include <stdio.h>
#include <math.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__10 = 10;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer i;
    static double t[10] = { -.9,-.7,-.5,-.3,-.1,.1,.3,.5,.7,.9 },
                  y[10] = { 81.,50.,35.,27.,26.,60.,106.,189.,318.,520. },
                  a[100] /* was [10][10] */, b[100] /* was [10][10] */,
                  x[10], deltay[10], chi2[10];
/* identify program to user */
    printf("%s\n\n", "Program E1REG demonstrates use of REGPOL.");
/* set errors */
    for (i = 1; i <= 10; ++i) {
	deltay[i - 1] = sqrt(y[i - 1]);
    }
/* perform polynomial regression */
    regpol_(t, y, deltay, &c__10, &c__10, x, b, a, chi2);
/* output */
    printf("%s\n", "T");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", t[i-1]);
    printf("\n");
    printf("%s\n", "Y");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", y[i-1]);
    printf("\n");
    printf("%s\n", "DELTAY");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", deltay[i-1]);
    printf("\n");
    printf("%s\n", "X");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", x[i-1]);
    printf("\n");
    printf("%s\n", "CHI2");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", chi2[i-1]);
    printf("\n");
    return 0;
} /* main */

