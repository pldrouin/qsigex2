#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2;

main(void)
{
    /* Initialized data */
    static double u[2] = { 3.,4. }, w[2] = { 1.,1. }, c, s;

/* identify program to user */
    printf("%s\n", "Program E3MTX demonstrates use of");
    printf("%s\n", "MTXGVD,MTXGVT and MTXGVA");
    printf("%s\n\n", "for Givens transformations.");
    printf("%s\n", "Output produced by program E3MTX.");
    printf("%s\n", "------------------------------------------");
/* write initial vectors */
    printf("%s\n", "U =");
    mtxwrt_(u, &c__1, &c__2);
    printf("%s\n", "W =");
    mtxwrt_(w, &c__1, &c__2);
    printf("%s\n\n", "------------------------------------------");
/* demonstrate MTXGVD */
    mtxgvd_(u, &u[1], &c, &s);
    printf("%s\n", "CALL MTXGVD(U(1),U(2),C,S) yields ");
    printf("%s%10.5lf%s%10.5lf\n\n", "C = ", c, ", S = ", s);
/* demonstrate MTXGVT */
    mtxgvt_(u, &u[1], &c, &s);
    printf("%s\n", "With the above values of C and S");
    printf("%s\n", "CALL MTXGVT(U(1),U(2),C,S) yields ");
    printf("%s\n", "U =");
    mtxwrt_(u, &c__1, &c__2);
    printf("\n");
/* demonstrate MTXGVA */
    mtxgva_(w, &w[1], &c, &s);
    printf("%s\n", "CALL MTXGVA(W(1),W(2),C,S) yields ");
    printf("%s%10.5lf%s%10.5lf\n", "C = ", c, ", S = ", s);
    printf("%s\n", "and W =");
    mtxwrt_(w, &c__1, &c__2);
    return 0;
} /* main */

