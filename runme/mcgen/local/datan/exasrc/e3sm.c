#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__75 = 75;
static integer c__60 = 60;

main(void)
{
    /* System generated locals */
    integer i__1;
    double d__1;

    /* Local variables */
    static integer i, n, npt, nws;
    static double rho, a[2], c[4] /* was [2][2] */,
                  x[2], sigma[2], dplus[4] /* was [2][2] */,
                  xa, xb, ya, yb, xs[1000], ys[1000];
    static char capt[75], tx[75], ty[75];

/* identify program to user */
    printf("%s\n", "Program E3SM demonstrates the use of SMSDGR");
    printf("%s\n", "by showing scatter diagram of sample");
    printf("%s\n\n", "from bivariate normal distribution.");
/* ask for input */
    printf("%s\n", "Enter a1:");
    printf("%s", "> ");
    scanf("%lf", &a[0]);
    printf("%s\n", "Enter a2:");
    printf("%s", "> ");
    scanf("%lf", &a[1]);
    printf("%s\n", "Enter sigma1 (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma[0]);
    printf("%s\n", "Enter sigma2 (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma[1]);
    printf("%s\n", "Enter rho (-1.<rho<1.):");
    printf("%s", "> ");
    scanf("%lf", &rho);
    printf("%s\n", "Enter number NPT of points (<1001):");
    printf("%s", "> ");
    scanf("%ld", &npt);
/* N=2 defines number of variables in multivariate */
/* normal distribution */
    n = 2;
/* set up covariance matrix */
/* Computing 2nd power */
    d__1 = sigma[0];
    c[0] = d__1 * d__1;
/* Computing 2nd power */
    d__1 = sigma[1];
    c[3] = d__1 * d__1;
    c[2] = rho * sigma[0] * sigma[1];
    c[1] = c[2];
/* prepare for generation of random numbers */
    rnmnpr_(c, dplus, &n);
/* generate sample of 2-vectors from bivariate normal */
    i__1 = npt;
    for (i = 1; i <= i__1; ++i) {
	rnmngn_(dplus, a, x, &n);
	xs[i - 1] = x[0];
	ys[i - 1] = x[1];
    }
/* prepare texts */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    strncpy(tx, "x_1", 3);
    strncpy(ty, "x_2", 3);
    sprintf(capt, "%s%6.2lf%s%6.2lf%s%5.2lf%s%5.2lf%s%5.2lf",
            "a_1#=", a[0], ", a_2#=", a[1], ", &s_1#=", sigma[0],
            ", &s_2#=", sigma[1], ", &r=", rho);
    capt[60] = ' ';
/* prepare scales */
    xa = a[0] - sigma[0] * 5.;
    xb = a[0] + sigma[0] * 5.;
    ya = a[1] - sigma[1] * 5.;
    yb = a[1] + sigma[1] * 5.;
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%ld", &nws);
    (void)getchar();
/* graphical output */
    smsdgr_(xs, ys, &npt, &xa, &xb, &ya, &yb, tx, &c__75, ty, &c__75, capt,
           &c__60, &nws);
    return 0;
} /* main */

