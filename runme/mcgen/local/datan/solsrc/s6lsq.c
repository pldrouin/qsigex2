#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern void LIBFUNC rnbw(double const P_T *a, double const P_T *gamma,
                         double P_T *r, integer const P_T *n);
extern double CBFUNC bwfnct(double const P_T *x, integer const P_T *nr,
                            double const P_T *t);

/* Table of constant values */

static double c_d0 = 0., c_d1 = 1., c_dm3 = -3.;

main(void)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */

    static integer i, n, ipl, nev, npl, lcapt, list[3], ltx, lty, nws,
                   nstep, nt, nr = 3;
    static double a[300] /* was [100][3] */, cx[9] /* was [3][3] */,
                  delt, hist[100], p, r, t[100], x[3], y[100], random[1000],
                  deltay[100], scrat[9] /* was [3][3] */, xpl[100], ypl[100];
    static char capt[75], tx[75], ty[75];

/* identify program to user */
    printf("%s\n", "Program S6LSQ generates data");
    printf("%s\n", "corresponding to a Breit-Wigner law");
    printf("%s\n\n", "and fits a Breit-Wigner to histogram.");
/* ask for input */
    printf("%s\n", "Enter number NEV (<= 1000)");
    printf("%s\n", "of events to be generated:");
    printf("%s", "> ");
    scanf("%li", &nev);
    printf("%s\n", "Enter number NT (<= 100) of histogram bins:");
    printf("%s", "> ");
    scanf("%li", &nt);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* generate events and build histogram */
    rnbw(&c_d0, &c_d1, random, &nev);
    delt = 6. / (double) nt;
    smhsin_(hist, &c_dm3, &delt, &nt);
    i__1 = nev;
    for (i = 1; i <= i__1; ++i) {
        smhsfl_(hist, &c_dm3, &delt, &nt, &random[i - 1], &c_d1);
    }
    n = 0;
    i__1 = nt;
    for (i = 1; i <= i__1; ++i) {
        if (hist[i - 1] > 0.) {
            ++n;
            t[n - 1] = ((double) i - .5) * delt - 3.;
            y[n - 1] = hist[i - 1];
            deltay[n - 1] = sqrt(y[n - 1]);
        }
    }
    if (n <= nr) {
        printf("%s%li%s\n", "only ", n ," bins with at least one event");
        exit(0);
    }
/* set first approximation of unknowns */
    x[0] = .5;
    x[1] = .5;
    x[2] = (double) nev;
/* perform fit */
    nstep = 100;
    lsqnon_(bwfnct, t, y, deltay, &n, &nr, &nr, list, x, cx, &r, a,
           scrat, &nstep);
    i__1 = n - nr;
    p = 1. - scchi2_(&r, &i__1);
    if (nstep < 0) {
        printf("%s%5li\n", "LSQNON ended with NSTEP = ", nstep);
    } else {
/* prepare graphics */
        memset(tx, ' ', 75);
        memset(ty, ' ', 75);
        strncpy(tx, "t", 1);
        strncpy(ty, "y", 1);
        ltx = 1;
        lty = 1;
        sprintf(capt, "x_1#=%5.2lf,x_2#=%5.2lf,x_3#=%5.1lf,"
                "&D@x_1#=%5.2lf,&D@x_2#=%5.2lf,&D@x_3#=%5.1lf",
                x[0], x[1], x[2], sqrt(cx[0]), sqrt(cx[4]), sqrt(cx[8]));
        lcapt = strlen(capt);
/* curve corresponding to solution */
        npl = 100;
        i__1 = npl;
        for (ipl = 1; ipl <= i__1; ++ipl) {
            xpl[ipl - 1] = (double) ipl * .08 - 4.;
            ypl[ipl - 1] = bwfnct(x, &nr, &xpl[ipl - 1]);
        }
/* graphical output */
        grhscv_(xpl, ypl, &npl, hist, &c_dm3, &delt, &nt, tx, &ltx, ty, &lty,
               capt, &lcapt, &nws);
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

/* program generates random numbers corresponding to */
/* a Breit-Wigner distribution */
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

/* ------------------------------------------------- */
double CBFUNC bwfnct(double const P_T *x, integer const P_T *nr,
                     double const P_T *t)
{
    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

    /* Parameter adjustments */
    --x;

    /* Function Body */
/* Computing 2nd power */
    d__1 = x[2];
/* Computing 2nd power */
    d__2 = *t - x[1];
/* Computing 2nd power */
    d__3 = x[2];
    ret_val = x[3] * 2. * (d__1 * d__1) / (x[2] * 3.14159f * (d__2 * d__2 *
            4. + d__3 * d__3));
    return ret_val;
} /* bwfnct */

