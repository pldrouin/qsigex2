#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__3 = 3, c__4 = 4;

main(void)
{
    /* Initialized data */

    static integer m, n;
    static double b[9] /* was [3][3] */ = { 1.,3.,5.,7.,11.,13.,17.,19.,23. },
                  c[6] /* was [3][2] */ = { 1.,1.,1.,1.,1.,1. },
                  a[9] /* was [3][3] */, r[9] /* was [3][3] */,
                  s[9] /* was [3][3] */, u[9] /* was [3][3] */;

/* demonstrates use of Cholesky routines */
/* identify program to user */
    printf("%s\n", "Program E6MTX demonstrates use of");
    printf("%s\n", "MTXCHL,MTXCHI and MTXCHM");
    printf("%s\n\n", "for Cholesky decomposition and inversion.");
/* construct symmetric, positive definite matrix X */
    n = 3;
    m = 2;
    mtxmat_(b, b, a, &n, &n, &n);
    printf("%s\n", "Output produced by program E6MTX.");
    printf("%s\n", "------------------------------------------");
/* write input data */
    printf("%s\n", "A =");
    mtxwrt_(a, &n, &n);
    printf("\n%s\n", "C =");
    mtxwrt_(c, &n, &m);
    printf("\n%s\n\n", "------------------------------------------");
/* demonstrate MTXCHL */
    mtxchl_(a, u, &n);
    printf("%s\n", "CALL MTXCHL(A,U,N) yields U = ");
    mtxwrt_(u, &n, &n);
    printf("\n");
/* perform check by multiplying U with its transposed */
    mtxmat_(u, u, r, &n, &n, &n);
    printf("%s\n", "check of decomposition: R = U^T U =");
    mtxwrt_(r, &n, &n);
    printf("\n");
/* demonstrate MTXCHM */
    mtxchm_(u, c, r, &n, &m);
    printf("%s\n", "CALL MTXCHM(U,C,R,N,M)  yields  R =");
    mtxwrt_(r, &n, &m);
    printf("\n");
/* demonstrate MTXCHI */
/* (first save original matrix A) */
    mtxtra_(a, s, &n, &n);
    mtxchi_(a, u, &n);
    printf("%s\n", "CALL MTXCHI(A,U,N) yields  A =");
    mtxwrt_(a, &n, &n);
    printf("\n");
/* perform check by multiplying original matrix A with inverse */
    mtxmlt_(s, a, r, &n, &n, &n);
    printf("%s\n", "check of inversion: R = A**(-1) A =");
    mtxwrt_(r, &n, &n);
    printf("\n");
    return 0;
} /* main */

