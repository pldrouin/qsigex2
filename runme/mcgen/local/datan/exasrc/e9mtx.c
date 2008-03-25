#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3;
static double c_d0 = 0., c_dp001 = .001;

main(void)
{
    /* Initialized data */
    static double a[9] /* was [3][3] */ = { 1.,2.,1.,2.,1.,1.,3.,-2.,2. },
                  b[3] /* was [3][1] */ = { 1.,0.,0. },
                  x1[3] /* was [3][1] */, x2[3] /* was [3][1] */;
    static logical ok;
    static integer nn;

/* identify program to user */
    printf("%s\n\n", "Program E9MTX demonstrates use of MTXMAR.");
    printf("%s\n", "Output produced by program E9MTX.");
    printf("%s\n", "------------------------------------------");
/* write input data */
    printf("%s%2li\n", "N = ", 3L);
/* write initial matrices */
    printf("%s\n", "A =");
    mtxwrt_(a, &c__3, &c__3);
    printf("%s\n", "B =");
    mtxwrt_(b, &c__3, &c__1);
    printf("%s\n\n", "------------------------------------------");
/* demonstrate MTXMAR */
    nn = c__3;
    mtxmar_(a, b, &c_dp001, x1, x2, &c__3, &nn, &c_d0, &ok);
    printf("%s\n", "CALL MTXMAR(A,B,ALAM,X1,X2,N,N,FRAC,OK)  yields ");
    printf("%s %li\n", "OK =", ok);
    printf("%s\n", "X1 =");
    mtxwrt_(x1, &c__3, &c__1);
    printf("%s\n", "X2 =");
    mtxwrt_(x2, &c__3, &c__1);
    return 0;
} /* main */

