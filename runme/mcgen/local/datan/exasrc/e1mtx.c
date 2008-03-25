#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__2 = 2;
static double c_dp5 = .5;

main(void)
{
    /* Initialized data */

    static double a[6] /* was [2][3] */ = { 1.,2.,2.,1.,3.,3. },
                  b[6] /* was [2][3] */ = { 2.,1.,3.,5.,1.,4. },
                  c[6] /* was [3][2] */ = { 1.,3.,2.,5.,4.,3. },
                  u[3] = { 0.,3.,4. }, v[3] = { 3.,1.,2. }, w[2] = { 5.,2. },
                  r[6] /* was [2][3] */, s[4] /* was [2][2] */,
                  t[9] /* was [3][3] */, x[3], z[2], sc, dot;

/* identify program to user */
    printf("%s\n", "Program E1MTX demonstrates use of");
    printf("%s\n\n", "simple routines for matrix and vector algebra.");
    printf("%s\n", "Output produced by program E1MTX.");
    printf("%s\n", "------------------------------------------");
/* write initial matrices and vectors */
    printf("%s\n", "J = 2, K = 3, FACT = 0.5");
    printf("%s\n", "A =");
    mtxwrt_(a, &c__2, &c__3);
    printf("%s\n", "B =");
    mtxwrt_(b, &c__2, &c__3);
    printf("%s\n", "C =");
    mtxwrt_(c, &c__3, &c__2);
    printf("%s\n", "U =");
    mtxwrt_(u, &c__1, &c__3);
    printf("%s\n", "V =");
    mtxwrt_(v, &c__1, &c__3);
    printf("%s\n", "W =");
    mtxwrt_(w, &c__1, &c__2);
    printf("%s\n\n", "------------------------------------------");
/* demonstrate MTXTRA */
    mtxtra_(a, r, &c__2, &c__3);
    printf("%s\n", "CALL MTXTRA(A,R,J,K) yields R =");
    mtxwrt_(r, &c__2, &c__3);
    printf("\n");
/* demonstrate MTXADD */
    mtxadd_(a, b, r, &c__2, &c__3);
    printf("%s\n", "CALL MTXADD(A,B,R,J,K) yields R =");
    mtxwrt_(r, &c__2, &c__3);
    printf("\n");
/* demonstrate MTXSUB */
    mtxsub_(a, b, r, &c__2, &c__3);
    printf("%s\n", "CALL MTXSUB(A,B,R,J,K) yields R =");
    mtxwrt_(r, &c__2, &c__3);
    printf("\n");
/* demonstrate MTXMLT */
    mtxmlt_(a, c, s, &c__2, &c__3, &c__2);
    printf("%s\n", "CALL MTXMLT(A,C,S,J,K,J) yields S =");
    mtxwrt_(s, &c__2, &c__2);
    printf("\n");
/* demonstrate MTXMBT */
    mtxmbt_(a, b, s, &c__2, &c__3, &c__2);
    printf("%s\n", "CALL MTXMBT(A,B,S,J,K,J) yields S =");
    mtxwrt_(s, &c__2, &c__2);
    printf("\n");
/* demonstrate MTXMAT */
    mtxmat_(a, b, t, &c__3, &c__2, &c__3);
    printf("%s\n", "CALL MTXMAT(A,B,T,K,J,K) yields T =");
    mtxwrt_(t, &c__3, &c__3);
    printf("\n");
/* demonstrate MTXUNT */
    mtxunt_(r, &c__2);
    printf("%s\n", "CALL MTXUNT(R,J) yields R =");
    mtxwrt_(r, &c__2, &c__2);
    printf("\n");
/* demonstrate MTXZER */
    mtxzer_(r, &c__2, &c__3);
    printf("%s\n", "CALL MTXZER(R,J,K) yields R =");
    mtxwrt_(r, &c__2, &c__3);
    printf("\n");
/* demonstrate MTXMSC */
    mtxmsc_(a, r, &c_dp5, &c__2, &c__3);
    printf("%s\n", "CALL MTXMSC(A,R,FACT,J,K) yields R =");
    mtxwrt_(r, &c__2, &c__3);
    printf("\n");
/* demonstrate MTXTRP */
    mtxtrp_(a, r, &c__2, &c__3);
    printf("%s\n", "CALL MTXTRP(A,R,J,K) yields R =");
    mtxwrt_(r, &c__3, &c__2);
    printf("\n");
/* demonstrate MTXCPV */
    mtxcpv_(w, z, &c__2);
    printf("%s\n", "CALL MTXCPV(W,Z,J) yields Z =");
    mtxwrt_(z, &c__1, &c__2);
    printf("\n");
/* demonstrate MTXADV */
    mtxadv_(u, v, x, &c__3);
    printf("%s\n", "CALL MTXADV(U,V,X,K) yields X =");
    mtxwrt_(x, &c__1, &c__3);
    printf("\n");
/* demonstrate MTXSBV */
    mtxsbv_(u, v, x, &c__3);
    printf("%s\n", "CALL MTXSBV(U,V,X,K) yields X =");
    mtxwrt_(x, &c__1, &c__3);
    printf("\n");
/* demonstrate MTXDOT */
    mtxdot_(u, v, &dot, &c__3);
    printf("%s%20.13g\n\n", "CALL MTXDOT(U,V,DOT,K) yields DOT =", dot);
/* demonstrate MTXNRV */
    mtxnrv_(u, &sc, &c__3);
    printf("%s%20.13g\n\n", "CALL MTXNRV(U,SC,K) yields SC =", sc);
/* demonstrate MTXMSV */
    mtxmsv_(u, x, &c_dp5, &c__3);
    printf("%s\n", "CALL MTXMSV(U,X,FACT,K) yields X =");
    mtxwrt_(x, &c__1, &c__3);
    printf("\n");
/* demonstrate MTXZRV */
    mtxzrv_(x, &c__3);
    printf("%s\n", "CALL MTXZRV(X,K) yields X =");
    mtxwrt_(x, &c__1, &c__3);
    printf("\n");
    return 0;
} /* main */

