#include <stdio.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3;
static double c_d1em10 = 1e-10;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer nsing, iswit, iswit1, i, m, n, norg;
    static double c1[3] = { 1.,2.,1. }, c2[3] = { 2.,1.,1. },
                  c3[3] = { 3.,-2.,2. },
                  a[9] /* was [3][3] */, b[9] /* was [3][3] */, c[3] , d[3],
                  r[3], s[9] /* was [3][3] */, u[9] /* was [3][3] */,
                  v[9] /* was [3][3] */, x[9] /* was [3][3] */;
    static logical ok;

/* identify program to user */
    printf("%s\n", "Program E8MTX demonstrates use of");
    printf("%s\n", "MTXDEC solving matrix equations in 9 different");
    printf("%s\n\n", "cases using singular value decomposition.");
    printf("%s\n", "Output produced by program E8MTX.");
    for (iswit = 1; iswit <= 3; ++iswit) {
	for (iswit1 = 1; iswit1 <= 3; ++iswit1) {
        printf("%s\n", "----------------------------------------");
	    if (iswit == 1) {
                printf("%s\n", "N=M (CASE 1)");
	    } else if (iswit == 2) {
                printf("%s\n", "N<M (CASE 2)");
	    } else if (iswit == 3) {
                printf("%s\n", "N>M (CASE 3)");
	    }
	    if (iswit1 == 1) {
                printf("%s\n", "A has full rank (CASE a)");
	    } else if (iswit1 == 2) {
                printf("%s\n", "A has smaller than full rank (CASE b)");
	    } else if (iswit1 == 3) {
                printf("%s\n", "A has smaller than full pseudorank (CASE c)");
	    }
/* prepare input */
	    if (iswit == 1) {
/* case 1 */
		m = 3;
		n = 3;
		mtxpcl_(a, c1, &m, &n, &c__1);
		mtxpcl_(a, c2, &m, &n, &c__2);
		mtxpcl_(a, c3, &m, &n, &c__3);
		if (iswit1 == 2) {
		    mtxpcl_(a, c2, &m, &n, &c__3);
		}
		if (iswit1 == 3) {
		    mtxcpv_(c2, c, &m);
		    c[2] += 1e-12;
		    mtxpcl_(a, c, &m, &n, &c__3);
		}
	    } else if (iswit == 2) {
/* case 2 */
		m = 3;
		n = 2;
		mtxpcl_(a, c1, &m, &n, &c__1);
		mtxpcl_(a, c2, &m, &n, &c__2);
		if (iswit1 == 2) {
		    mtxpcl_(a, c1, &m, &n, &c__2);
		}
		if (iswit1 == 3) {
		    mtxcpv_(c1, c, &m);
		    c[2] += 1e-12;
		    mtxpcl_(a, c, &m, &n, &c__2);
		}
	    } else if (iswit == 3) {
/* case 3 */
/* (the case (M=2, N=3)is treated by constructing adding a line of */
/*  zeroes to the original 2x3 matrix A and tresting it as a 3x3 matrix) */
		m = 3;
		n = 3;
		mtxzer_(a, &m, &n);
		mtxprw_(a, c1, &m, &n, &c__1);
		mtxprw_(a, c2, &m, &n, &c__2);
		if (iswit1 == 2) {
		    mtxprw_(a, c1, &m, &n, &c__2);
		} else if (iswit1 == 3) {
		    mtxcpv_(c1, c, &m);
		    c[2] += 1e-12;
		    mtxprw_(a, c, &m, &n, &c__2);
		}
	    }
/* write initial matrix and its dimensions */
            printf("%s%8.1lG%s%8.1lG\n", "EPSILN = ", 1e-12,
                   ", FRAC = ", c_d1em10);
            printf("%s%2li%s%2li\n", "M = ", m, ", N = ", n);
            printf("%s\n", "A =");
	    mtxwrt_(a, &m, &n);
            printf("%s\n\n", "------------------------------------------");
/* demonstrate MTXDEC */
	    norg = n;
	    mtxdec_(a, b, x, r, &m, &n, &c_d1em10, &ok, d, u, v);
            printf("%s\n", "CALL MTXDEC(A,B,X,R,M,N,FRAC,OK,D,U,V) yields");
	    nsing = n;
            printf("%s%li\n", "number of non zero singular values is ", nsing);
	    n = norg;
            printf("%s %li\n", "OK =", ok);
            printf("%s\n", "X =");
	    mtxwrt_(x, &n, &m);
            printf("%s\n", "R =");
	    mtxwrt_(r, &c__1, &m);
            printf("%s\n", "D =");
	    mtxwrt_(d, &c__1, &n);
/* perform singular value analysis (SVA) */
	    if (nsing < norg) {
		for (i = nsing + 1; i <= norg; ++i) {
		    d[i - 1] = 0.;
		}
                printf("%s\n", "after singular value analysis (SVA): D =");
		mtxwrt_(d, &c__1, &n);
	    }
            printf("%s\n", "U =");
	    mtxwrt_(u, &m, &m);
            printf("%s\n", "V =");
	    mtxwrt_(v, &n, &n);
/* perform check by forming matrix product U*D*V^T */
	    mtxzer_(s, &m, &m);
	    for (i = 1; i <= n; ++i) {
		s[i + i * 3 - 4] = d[i - 1];
	    }
	    mtxmlt_(u, s, b, &m, &m, &n);
	    mtxmbt_(b, v, u, &m, &n, &n);
            printf("%s\n", "check of SVD: U*D*V^T =");
	    mtxwrt_(u, &m, &n);
	}
    }
    return 0;
} /* main */

