#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern double CBFUNC powerl(double const P_T *x, integer const P_T *nr,
                            double const P_T *t);

/* Table of constant values */

static integer c__1 = 1;
static double c_dp5 = .5;

main(void)
{
    /* Local variables */
    static integer i, n, ipl, lcapt, list[2], npos, ltx, lty, nws, npl, nstep,
                   nr = 2;
    static double a[200] /* was [100][2] */, cx[4] /* was [2][2] */,
                  dellog[100], deltat, datcov[100], deltay[100], dt, dx1, dx2,
                  p, r, t[100], x[2], y[100], sigma, rho,
                  scrat[4] /* was [2][2] */, datsx[100], t0, tlog[100],
                  ylog[100], xpl[200], ypl[200];
    static char capt[75], tx[75], ty[75];
    static logical ok;
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S3LSQ generates data");
    printf("%s\n\n", "and fits a power law (nonlinear case).");
/* ask for input */
    printf("%s\n", "Enter number n of data points (3 <= n <= 100):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter first value T0 of controlled variable:");
    printf("%s", "> ");
    scanf("%lf", &t0);
    printf("%s\n", "Enter step width DELTAT of controlled variable:");
    printf("%s", "> ");
    scanf("%lf", &deltat);
    printf("%s\n", "Enter coefficient X(1):");
    printf("%s", "> ");
    scanf("%lf", &x[0]);
    printf("%s\n", "Enter exponent X(2):");
    printf("%s", "> ");
    scanf("%lf", &x[1]);
    printf("%s\n", "Enter size of measurement errors (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
/* generate data points corresponding to power law */
    rnstnr_(deltay, &n);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        t[i - 1] = t0 + (double) (i - 1) * deltat;
        y[i - 1] = powerl(x, &nr, &t[i - 1]);
        y[i - 1] += deltay[i - 1] * sigma;
        deltay[i - 1] = sigma;
    }
/* find first approximation of unknowns by straight line in log-log plot */
    npos = 0;
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        if (t[i - 1] > 0. && y[i - 1] > 0.) {
            ++npos;
            tlog[npos - 1] = log(t[i - 1]);
            ylog[npos - 1] = log(y[i - 1]);
            dellog[npos - 1] = 1.;
        }
    }
    lsqpol_(tlog, ylog, dellog, &npos, &nr, x, cx, &r, a, scrat, &ok);
    x[0] = exp(x[0]);
/* perform fit */
    nstep = 100;
    lsqnon_(powerl, t, y, deltay, &n, &nr, &nr, list, x, cx, &r, a,
           scrat, &nstep);
    if (n != nr) {
        i__1 = n - nr;
        p = 1. - scchi2_(&r, &i__1);
        dx1 = sqrt(cx[0]);
        dx2 = sqrt(cx[3]);
        rho = cx[2] / (dx1 * dx2);
    }
/* prepare graphics */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    strncpy(tx, "t", 1);
    strncpy(ty, "y", 1);
    ltx = 1;
    lty = 1;
    sprintf(capt, "x_1#=%6.2lf, x_2#=%6.2lf", x[0], x[1]);
    lcapt = strlen(capt);
    if (n != nr) {
        sprintf(capt + 24, ", &D@x_1#=%6.2lf, &D@x_2#=%6.2lf, &r@=%6.2lf",
                dx1, dx2, rho);
        lcapt = strlen(capt);
    }
/* curve corresponding to solution */
    npl = 200;
    dt = (double) (n - 1) / (double) (npl - 1) * deltat;
    i__1 = npl;
    for (ipl = 1; ipl <= i__1; ++ipl) {
        xpl[ipl - 1] = t0 + dt * (double) ipl;
        ypl[ipl - 1] = powerl(x, &nr, &xpl[ipl - 1]);
    }
/* prepare data points for graphical presentation */
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        datsx[i - 1] = 0.;
        datcov[i - 1] = 0.;
    }
/* graphical output */
    grdtcv_(xpl, ypl, &npl, &c__1, &c_dp5, t, y, datsx, deltay, datcov, &n,
           tx, &ltx, ty, &lty, capt, &lcapt, &nws);
    return 0;
} /* main */

/* --------------------------------------------- */
double CBFUNC powerl(double const P_T *x, integer const P_T *nr,
                    double const P_T *t)
{
    /* System generated locals */
    double ret_val;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    ret_val = x[1] * pow(*t, x[2]);
    return ret_val;
} /* powerl */

