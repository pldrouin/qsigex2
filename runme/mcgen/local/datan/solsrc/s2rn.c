#include <stdio.h>
#include <math.h>
#include "datsrc.h"
#include "grsrc.h"

extern void LIBFUNC rntrit(double const P_T *a, double const P_T *b,
                           double const P_T *c, double P_T *r,
                           integer const P_T *n);
extern void LIBFUNC rntrir(double const P_T *a, double const P_T *b,
                           double const P_T *c, double P_T *r,
                           integer const P_T *n);

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__5 = 5;
static double c_d1 = 1.;

main(void)
{
    /* Initialized data */
    static char tx[5] = "x    ", ty[5] = "N(x) ", capt[5] = "     ";
    /* Local variables */
    static integer i, n, itype, iswit, nws, nx;
    static double delx, hist[50], a, b, c, r[1000], x0;
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S2RN demonstrates use of RNTRIT and RNTRIR");
    printf("%s\n", "which are subroutines generating random numbers");
    printf("%s\n\n", "following a triangular distribution.");
/* let user choose */
    printf("%s\n", "Enter number N (1<= N <= 1000):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter A:");
    printf("%s", "> ");
    scanf("%lf", &a);
    printf("%s\n", "Enter B:");
    printf("%s", "> ");
    scanf("%lf", &b);
    printf("%s\n", "Enter C (A<C<B):");
    printf("%s", "> ");
    scanf("%lf", &c);
    printf("%s\n", "Do you want");
    printf("%s\n", "1 - numerical output");
    printf("%s\n", "2 - graphical output");
    printf("%s", "> ");
    scanf("%li", &iswit);
    printf("%s\n", "Do you want");
    printf("%s\n", "1 - generation by transformation method");
    printf("%s\n", "2 - generation by rejection method");
    printf("%s", "> ");
    scanf("%li", &itype);
    if (itype == 1) {
        rntrit(&a, &b, &c, r, &n);
    }
    if (itype == 2) {
        rntrir(&a, &b, &c, r, &n);
    }
    if (iswit == 1) {
/* numerical output */
        printf("%s\n", "Random numbers are");
        mtxwrt_(r, &c__1, &n);
    } else if (iswit == 2) {
/* graphical output */
/* initialize histogram */
        x0 = a;
        delx = (b - a) * .02;
        nx = 50;
        smhsin_(hist, &x0, &delx, &nx);
/* fill histogram */
        i__1 = n;
        for (i = 1; i <= i__1; ++i) {
            smhsfl_(hist, &x0, &delx, &nx, &r[i - 1], &c_d1);
        }
/* ask for number of workstation */
        grnbws_();
        printf("%s\n", "Please, enter number of workstation:");
        printf("%s", "> ");
        scanf("%li", &nws);
        (void)getchar();
/* display of histogram */
        smhsgr_(hist, &x0, &delx, &nx, tx, &c__5, ty, &c__5, capt, &c__5, &nws);
    }
    return 0;
} /* main */

/* ------------------------------------------------------------------ */
void LIBFUNC rntrit(double const P_T *a, double const P_T *b,
                    double const P_T *c, double P_T *r, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    static double xlim, ba, ca;

/* generation of triangularly distributed random numbers */
/* by transformation method */
    /* Parameter adjustments */
    --r;

    /* Function Body */
    ba = *b - *a;
    ca = *c - *a;
    xlim = ca / ba;
    rnecuy_(&r[1], n);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if (r[i] <= xlim) {
            r[i] = *a + sqrt(ba * ca * r[i]);
        } else {
            r[i] = *b - sqrt(ba * (*b - *c) * (1. - r[i]));
        }
    }
    return;
} /* rntrit */

/* ------------------------------------------------------------------ */
void LIBFUNC rntrir(double const P_T *a, double const P_T *b,
                    double const P_T *c, double P_T *r, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    static double h, r1, rr[2];

/* generation of triangularly distributed random numbers */
/* by rejection method */
    /* Parameter adjustments */
    --r;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
L10:
        rnecuy_(rr, &c__2);
        r1 = *a + rr[0] * (*b - *a);
        if (r1 <= *c) {
            h = (r1 - *a) / (*c - *a);
        } else {
            h = (*b - r1) / (*b - *c);
        }
        if (rr[1] >= h) {
            goto L10;
        }
        r[i] = r1;
    }
    return;
} /* rntrir */

