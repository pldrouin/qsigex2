#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__3 = 3;

main(void)
{
    /* Initialized data */

    static double a[9] /* was [3][3] */ = { 1.,2.,1.,2.,1.,1.,3.,-2., 2. },
                  b[9] /* was [3][3] */ = { 1.,0.,0.,0.,1.,0.,0.,0., 1. };

/* demonstrates solution of matrix equation by Gauss algorithm */
/* identify program to user */
    printf("%s\n\n", "Program E5MTX demonstrates use of MTXEQU.");
    printf("%s\n", "Output produced by program E5MTX.");
    printf("%s\n", "-----------------------------------------");
/* write input data */
    printf("%s%2li%s%2li\n", "N = ", c__3, ", M = ", c__3);
/* write initial matrices */
    printf("%s\n", "A =");
    mtxwrt_(a, &c__3, &c__3);
    printf("%s\n", "B =");
    mtxwrt_(b, &c__3, &c__3);
    printf("%s\n\n", "-----------------------------------------");
/* demonstrate MTXEQU */
    mtxequ_(a, b, &c__3, &c__3);
    printf("%s\n", "CALL MTXEQU(A,B,N,M) yields ");
    printf("%s\n", "B =");
    mtxwrt_(b, &c__3, &c__3);
    return 0;
} /* main */

