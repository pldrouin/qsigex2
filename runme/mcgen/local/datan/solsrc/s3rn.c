#include <stdio.h>
#include "datsrc.h"
#include "grsrc.h"

extern void LIBFUNC rnlndf(double const P_T *a, double const P_T *b,
                           double const P_T *t0, double const P_T *dt,
                           integer const P_T *n, double const P_T *sigmin,
                           double const P_T *sigmax, double P_T *t,
                           double P_T *y, double P_T *sigmay);

/* Table of constant values */

static integer c__1 = 1, c__2 = 2;
static double c_dp2 = .2;

main(void)
{
    /* Local variables */
    static integer i, n, nws;
    static double t[100], y[100], datsx[100], datsy[100], t0, dt, datcov[100],
                  a, b, sigmin, sigmax, xpl[2], ypl[2];
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S3RN demonstrates use of RNLNDF");
    printf("%s\n", "which is a subroutine generating data points");
    printf("%s\n", "along a straight line");
    printf("%s\n\n", "with errors of different size.");
/* ask for numerical input */
    printf("%s\n", "Enter number of points N (N<101):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter A:");
    printf("%s", "> ");
    scanf("%lf", &a);
    printf("%s\n", "Enter B:");
    printf("%s", "> ");
    scanf("%lf", &b);
    printf("%s\n", "Enter T0:");
    printf("%s", "> ");
    scanf("%lf", &t0);
    printf("%s\n", "Enter DT:");
    printf("%s", "> ");
    scanf("%lf", &dt);
    printf("%s\n", "Enter SIGMIN (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigmin);
    printf("%s\n", "Enter SIGMAX (>SIGMIN):");
    printf("%s", "> ");
    scanf("%lf", &sigmax);
/* simulate data points */
    rnlndf(&a, &b, &t0, &dt, &n, &sigmin, &sigmax, t, y, datsy);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	datsx[i - 1] = 0.;
	datcov[i - 1] = 0.;
    }
    xpl[0] = t0 - dt;
    xpl[1] = t0 + n * dt;
    ypl[0] = a * xpl[0] + b;
    ypl[1] = a * xpl[1] + b;
/* graphics */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
    grdtcv_(xpl, ypl, &c__2, &c__1, &c_dp2, t, y, datsx, datsy, datcov, &n, 
	   "t", &c__1, "y", &c__1, " ", &c__1, &nws);
    return 0;
} /* main */

/* ----------------------------------------------------------- */
void LIBFUNC rnlndf(double const P_T *a, double const P_T *b,
                    double const P_T *t0, double const P_T *dt,
                    integer const P_T *n, double const P_T *sigmin,
                    double const P_T *sigmax, double P_T *t,
                    double P_T *y, double P_T *sigmay)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    static double r;

/* generates data points (T(I),Y(I)) with errors of different size */
    /* Parameter adjustments */
    --sigmay;
    --y;
    --t;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	t[i] = *t0 + (i - 1) * *dt;
	y[i] = *a * t[i] + *b;
	rnecuy_(&r, &c__1);
	sigmay[i] = *sigmin + r * (*sigmax - *sigmin);
	rnstnr_(&r, &c__1);
	y[i] += r * sigmay[i];
    }
    return;
} /* rnlndf */

