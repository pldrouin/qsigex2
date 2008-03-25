#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1;
static double c_dp5 = .5;

main(void)
{
    /* Local variables */
    static integer i, n, ipl, npl, lcapt, ltx, lty, nws, nr = 1;
    static double a[100] /* was [100][1] */, c[100], cx[1] /* was [1][1] */,
                  deltat, datcov[100], deltay[100], dt, p, r, t[100], w, x[1],
                  y[100], sigma, scrat[1] /* was [1][1] */, datsx[100], t0,
                  xpl[200], ypl[200];
    static char capt[75], tx[75], ty[75];
    static logical ok;
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S2LSQ generates data");
    printf("%s\n\n", "and fits a power law (linear case).");
/* ask for input */
    printf("%s\n", "Enter number n of data points (2 <= n <= 100)):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter first value T0 of controlled variable:");
    printf("%s", "> ");
    scanf("%lf", &t0);
    printf("%s\n", "Enter step width DELTAT of controlled variable:");
    printf("%s", "> ");
    scanf("%lf", &deltat);
    printf("%s\n", "Enter coefficient X:");
    printf("%s", "> ");
    scanf("%lf", &x[0]);
    printf("%s\n", "Enter exponent W:");
    printf("%s", "> ");
    scanf("%lf", &w);
    printf("%s\n", "Enter size of measurement errors:");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* generate data points corresponding to power law */
    rnstnr_(deltay, &n);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        t[i - 1] = t0 + (double) (i - 1) * deltat;
        y[i - 1] = x[0] * pow(t[i - 1], w);
        y[i - 1] += deltay[i - 1] * sigma;
        deltay[i - 1] = sigma;
    }
/* set up A and C */
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        c[i - 1] = -y[i - 1];
        a[i - 1] = -pow(t[i - 1], w);
    }
/* perform fit */
    lsqlin_(t, c, deltay, &n, &nr, x, cx, &r, a, scrat, &ok);
/* compute chi**2 probability */
    if (1 == n) {
        p = 0.;
    } else {
        i__1 = n - 1;
        p = 1. - scchi2_(&r, &i__1);
    }
/* prepare graphics */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    strncpy(tx, "t", 1);
    strncpy(ty, "y", 1);
    ltx = 1;
    lty = 1;
    sprintf(capt, "x#=%6.2lf, &D@x=%6.2lf, M=%6.2lf, P=%6.2lf",
            x[0], sqrt(cx[0]), r, p);
    lcapt = strlen(capt);
/* curve corresponding to solution */
    npl = 200;
/* CCC          DT=(DBLE(N-1)/DBLE(NPL-1))*DELTAT */
    dt = (double) n / (double) (npl - 1) * deltat;
    i__1 = npl;
    for (ipl = 1; ipl <= i__1; ++ipl) {
        xpl[ipl - 1] = t0 + dt * (double) ipl;
        ypl[ipl - 1] = x[0] * pow(xpl[ipl - 1], w);
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

