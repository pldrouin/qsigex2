#include <stdio.h>
#include <math.h>
#include "datsrc.h"
#include "grsrc.h"

extern void LIBFUNC rnbw(double const P_T *a, double const P_T *gamma,
                         double P_T *r, integer const P_T *n);

/* Table of constant values */

static integer c__5 = 5;
static double c_d1 = 1.;

main(void)
{
    /* Initialized data */
    static char tx[5] = "x    ", ty[5] = "N(x) ", capt[5] = "     ";
    /* Local variables */
    static integer i, n, iswit, nws, nx;
    static double a, delx, hist[50], r[1000], gamma, x0;
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S1RN demonstrates use of RNBW");
    printf("%s\n", "which is a subroutine generating random numbers");
    printf("%s\n\n", "following a Breit-Wigner distribution.");
/* let user choose */
    printf("%s\n", "Enter number N (<1001):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter mean value A:");
    printf("%s", "> ");
    scanf("%lf", &a);
    printf("%s\n", "Enter full width at half maximum GAMMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &gamma);
    printf("%s\n", "Do you want");
    printf("%s\n", "1 - numerical output");
    printf("%s\n", "2 - graphical output");
    printf("%s", "> ");
    scanf("%li", &iswit);
    rnbw(&a, &gamma, r, &n);
    if (iswit == 1) {
/* numerical output */
        for (i = 1; i <= n; ++i) {
            if (i % 5 == 1)
                printf(" ");
            printf("%12.6lf", r[i-1]);
            if (i % 5 == 0)
               printf("\n");
        }
        if (n % 5 != 0)
           printf("\n");
    } else if (iswit == 2) {
/* graphical output */
/* initialize histogram */
	x0 = a - gamma * 5.;
	delx = gamma * .2;
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
void LIBFUNC rnbw(double const P_T *a, double const P_T *gamma,
                  double P_T *r, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;

/* a CALL of this subroutine generates in Array R a total of N */
/* random numbers following a Breit-Wigner distribution */
/* with mean A and full width at half maximum GAMMA */
    /* Parameter adjustments */
    --r;

    /* Function Body */
    rnecuy_(&r[1], n);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	r[i] = *a + *gamma * .5 * tan((r[i] - .5) * 3.14159);
    }
    return;
} /* rnbw */

