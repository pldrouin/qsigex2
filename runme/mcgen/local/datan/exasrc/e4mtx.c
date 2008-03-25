#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__5 = 5, c__6 = 6;

main(void)
{
    /* Initialized data */

    static double v[6] = { 1.,2.,0.,4.,3.,4. },
                  c[6] = { 1.,2.,0.,4.,3.,4. }, b, up;

/* identify program to user */
    printf("%s\n", "Program E4MTX demonstrates use of");
    printf("%s\n", "MTXHSD and MTXHST");
    printf("%s\n\n", "for Householder transformations.");
    printf("%s\n", "Output produced by program E4MTX.");
    printf("%s\n", "------------------------------------------");
/* write input data */
    printf("%s%2li%s%2li%s%2li\n", "N = ", c__6, ", LP = ", c__3,
           ", L = ", c__5);
/* write initial vector */
    printf("%s\n", "V =");
    mtxwrt_(v, &c__1, &c__6);
    printf("%s\n\n", "------------------------------------------");
/* demonstrate MTXHSD */
    mtxhsd_(v, &up, &b, &c__6, &c__3, &c__5);
    printf("%s\n", "CALL MTXHSD(U,UP,B,N,LP,L) yields ");
    printf("%s%10.5lf%s%10.5lf\n\n", "UP = ", up, ", B = ", b);
/* demonstrate MTXHST */
    mtxhst_(v, &up, &b, c, &c__6, &c__3, &c__5);
    printf("%s\n", "With the above values of V, UP and B");
    printf("%s\n", "CALL MTXHST(V,UP,B,C,N,LP,L) yields ");
    printf("%s\n", "C =");
    mtxwrt_(c, &c__1, &c__6);
    return 0;
} /* main */

