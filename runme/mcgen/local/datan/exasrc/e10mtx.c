#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3;
static double c_d0 = 0.;

main(void)
{
    /* Initialized data */
    static double a[9] /* was [3][3] */ = { 1.,0.,0.,0.,1.,0.,0.,0.,1. },
                  b[3] = { 89.,31.,62. }, d[1] = { 180. },
                  e[3] /* was [1][3] */ = { 1.,1.,1. }, r, x[3],
                  a2[6] /* was [3][2] */;
    static logical ok;
/* identify program to user */
    printf("%s\n", "Program E10MTX demonstrates use of MTXLSC");
    printf("%s\n", "to solve a least squares problem");
    printf("%s\n\n", "with linear constraints.");
    printf("%s\n", "Output produced by program E10MTX.");
    printf("%s\n", "------------------------------------------");

/* write input data */
    printf("%s%2li\n", "N = ", c__3);
/* write initial matrices */
    printf("%s\n", "A =");
    mtxwrt_(a, &c__3, &c__3);
    printf("%s\n", "B =");
    mtxwrt_(b, &c__3, &c__1);
    printf("%s\n", "D =");
    mtxwrt_(d, &c__1, &c__1);
    printf("%s\n", "E =");
    mtxwrt_(e, &c__1, &c__3);
    printf("%s\n\n", "------------------------------------------");
/* demonstrate MTXLSC */
    mtxlsc_(a, b, e, d, x, &r, a2, &c__3, &c__3, &c__1, &c_d0, &ok);
    printf("%s\n", "CALL MTXLSC(A,B,E,D,X,R,A2,M,N,L,FRAC,OK)   yields");
    printf("%s %li\n", "OK =", ok);
    printf("%s\n", "X =");
    mtxwrt_(x, &c__1, &c__3);
    printf("%s%15.10lf\n", "R = ", r);
    return 0;
} /* main */

