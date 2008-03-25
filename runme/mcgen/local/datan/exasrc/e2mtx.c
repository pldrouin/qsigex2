#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3;

main(void)
{
    /* Initialized data */

    static integer list[3] = { 1,0,1 };
    static double a[6] /* was [2][3] */ = { 1.,2.,2.,1.,3.,3. },
                  d[4] /* was [2][2] */ = { 0.,1.,2.,3. },
                  u[3] = { 0.,3.,4. }, w[2] = { 5.,2. },
                  r[6] /* was [2][3] */, s[4] /* was [2][2] */, x[3], z[2];

/* identify program to user */
    printf("%s\n", "Program E2MTX demonstrates use of routines");
    printf("%s\n\n", "for manipulation of submatrices and subvectors.");
    printf("%s\n", "Output produced by program E2MTX.");
    printf("%s\n", "------------------------------------------");
/* write initial matrices and vectors */
    printf("%s\n", "J = 2, K = 3, LIST = (1,0,1), FACT = 0.5");
    printf("%s\n", "A =");
    mtxwrt_(a, &c__2, &c__3);
    printf("%s\n", "D =");
    mtxwrt_(d, &c__2, &c__2);
    printf("%s\n", "U =");
    mtxwrt_(u, &c__1, &c__3);
    printf("%s\n", "W =");
    mtxwrt_(w, &c__1, &c__2);
    printf("%s\n", "------------------------------------------");
    printf("\n");
/* demonstrate MTXGSM */
    mtxgsm_(a, s, &c__2, &c__3, &c__2, &c__2, &c__1, &c__2);
    printf("%s\n", "CALL MTXGSM(A,S,J,K,J,J,1,2) yields S =");
    mtxwrt_(s, &c__2, &c__2);
    printf("\n");
/* demonstrate MTXPSM */
/* (first save and later restore original matrix A) */
    mtxtra_(a, r, &c__2, &c__3);
    mtxpsm_(a, d, &c__2, &c__3, &c__2, &c__2, &c__1, &c__1);
    printf("%s\n", "CALL MTXPSM(A,D,J,K,J,J,1,1) yields A =");
    mtxwrt_(a, &c__2, &c__3);
    printf("\n");
    mtxtra_(r, a, &c__2, &c__3);
/* demonstrate MTXGCL */
    mtxgcl_(a, x, &c__2, &c__3, &c__2);
    printf("%s\n", "CALL MTXGCL(A,X,J,K,2) yields X =");
    mtxwrt_(x, &c__2, &c__1);
    printf("\n");
/* demonstrate MTXPCL */
/* (first save and later restore original matrix A) */
    mtxtra_(a, r, &c__2, &c__3);
    mtxpcl_(a, w, &c__2, &c__3, &c__1);
    printf("%s\n", "CALL MTXPCL(A,W,J,K,1) yields A =");
    mtxwrt_(a, &c__2, &c__3);
    printf("\n");
    mtxtra_(r, a, &c__2, &c__3);
/* demonstrate MTXGRW */
    mtxgrw_(a, x, &c__2, &c__3, &c__2);
    printf("%s\n", "CALL MTXGRW(A,X,J,K,2) yields X =");
    mtxwrt_(x, &c__1, &c__3);
    printf("\n");
/* demonstrate MTXPRW */
/* (first save and later restore original matrix A) */
    mtxtra_(a, r, &c__2, &c__3);
    mtxprw_(a, u, &c__2, &c__3, &c__1);
    printf("%s\n", "CALL MTXPRW(A,U,J,K,1) yields A =");
    mtxwrt_(a, &c__2, &c__3);
    printf("\n");
/* demonstrate MTXGSV */
    mtxgsv_(u, z, &c__3, &c__2, list);
    printf("%s\n", "CALL MTXGSV(U,Z,K,J,LIST) yields Z =");
    mtxwrt_(z, &c__1, &c__2);
    printf("\n");
/* demonstrate MTXPSV */
    mtxpsv_(u, w, &c__3, &c__2, list);
    printf("%s\n", "CALL MTXPSV(U,W,K,J,LIST) yields U =");
    mtxwrt_(u, &c__1, &c__3);
    printf("\n");
    return 0;
} /* main */

