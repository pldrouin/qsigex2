#include <stdio.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1;

main(void)
{
    /* Local variables */
    static integer i, n, iswit, nws;
    static double a, b, t[100], y[100], t0, dt, xa, xb, ya, yb, sigmay;
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n\n", "Program E2RN demonstrates use of RNLINE.");
/* ask for numerical input */
    printf("%s\n", "Enter number of points N (2 <= N <= 100):");
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
    printf("%s\n", "Enter SIGMAY (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigmay);
/* simulate data points */
    rnline_(&a, &b, &t0, &dt, &n, &sigmay, t, y);
/* ask for type of output */
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - Numerical Output");
    printf("%s\n", "2 - Graphical Output");
    printf("%s", "> ");
    scanf("%li", &iswit);
    if (iswit == 1) {
/* numerical output */
        printf("%s%7.2lf%s%7.2lf%s%7.2lf%s%7.2lf%s%7.2lf\n",
               "A = ", a, ", B = ", b, ", T0 = ", t0,
               ", DT = ", dt, ", SIGMAY = ", sigmay);
        printf("%s\n", "Pairs T(I), Y(I) produced by RNLINE:");
        i__1 = n;
        for (i = 1; i <= i__1; ++i) {
            printf("%15.5lG%15.5lG\n", t[i-1], y[i-1]);
        }
    } else if (iswit == 2) {
/* graphical output */
        grnbws_();
        printf("%s\n", "Please, enter number of workstation:");
        printf("%s", "> ");
        scanf("%li", &nws);
        (void)getchar();
        xa = t0 - 1.;
        xb = t0 + n * dt;
        ya = a * t0 + b - (a >= 0. ? 5. : -5.) * sigmay;
        yb = a * (t0 + (n - 1) * dt) + b + (a >= 0. ? 5. : -5.) * sigmay;
        smsdgr_(t, y, &n, &xa, &xb, &ya, &yb, "t", &c__1, "y", &c__1,
               " ", &c__1, &nws);
    }
    return 0;
} /* main */

