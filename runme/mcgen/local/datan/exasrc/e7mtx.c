#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3;
static double c_d0 = 0.;

main(void)
{
    /* Initialized data */
    static logical ok;
    static double a[9] /* was [3][3] */ = { 1.,2.,1.,2.,1.,1.,3.,-2.,2. },
                  b[9] /* was [3][3] */ = { 1.,0.,0.,0.,1.,0.,0.,0.,1. },
                  r[3], x[9] /* was [3][3] */;
    static integer nn;

/* demonstrates solution of matrix equation */
/* by singular value decomposition (SVD) */
/* identify program to user */
    printf("%s\n", "Program E7MTX demonstrates use of");
    printf("%s\n\n", "MTXSVD for singular value decomposition.");
    printf("%s\n", "Output produced by program E7MTX.");
    printf("%s\n", "------------------------------------------");
/* write input data */
    printf("%s%2li%s%2li\n", "N = ", c__3, ", M = ", c__3);
/* write initial matrices */
    printf("%s\n", "A =");
    mtxwrt_(a, &c__3, &c__3);
    printf("%s\n", "B =");
    mtxwrt_(b, &c__3, &c__3);
    printf("%s\n\n", "------------------------------------------");
/* demonstrate MTXSVD */
    nn = c__3;
    mtxsvd_(a, b, x, r, &c__3, &nn, &c__3, &c_d0, &ok);
    printf("%s\n", "CALL MTXSVD(A,B,X,R,N,N,M,FRAC,OK) yields");
    printf("%s%li\n", "OK =", ok);
    printf("%s\n", "X =");
    mtxwrt_(x, &c__3, &c__3);
    printf("%s\n", "R =");
    mtxwrt_(r, &c__1, &c__3);
    return 0;
} /* main */

