#include <stdio.h>
#include <math.h>
#include <string.h>
#include "grsrc.h"
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__9 = 9;

main()
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i, lcapt, nx, npl, nws, ltx, lty;
    static double delx, hist[100], sigma, x0, alambd, dx, xpl[1000], ypl[1000];
    static char capt[75], tx[75], ty[75];

/* identify program to user */
    (void)printf("%s\n\n", "Program E6GR demonstrates use of GRHSCV.");
/* ask for number of workstation */
    grnbws_();
    (void)printf("%s\n", "Please, enter number of workstation:");
    (void)printf("%s", "> ");
    (void)scanf("%ld", &nws);
    (void)getchar();
    alambd = 10.;
/* prepare histogram */
    x0 = -.5;
    nx = 30;
    delx = 1.;
/* fill histogram with Poisson probabilities */
    i__1 = nx;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i - 1;
	hist[i - 1] = sdpois_(&i__2, &alambd);
    }
/* prepare polyline */
    npl = 1000;
    dx = (double) nx * delx / (double) (npl - 1);
    sigma = sqrt(alambd);
/* compute points on polyline with normal probability density */
    i__1 = npl;
    for (i = 1; i <= i__1; ++i) {
	xpl[i - 1] = x0 + (double) (i - 1) * dx;
	ypl[i - 1] = sdnorm_(&xpl[i - 1], &alambd, &sigma);
    }
/* prepare texts and caption */
    (void)memset(tx, ' ', 75);
    (void)memset(ty, ' ', 75);
    (void)memset(capt, ' ', 75);
    (void)strncpy(tx, "k", 1);
    ltx = 1;
    (void)strncpy(ty, "P(k)", 4);
    lty = 4;
    (void)strncpy(capt, "Poisson (histogram) and Gaussian (cont. line)", 45);
    lcapt = 45;
/* produce graphics by call of GRHSCV */
    grhscv_(xpl, ypl, &npl, hist, &x0, &delx, &nx, tx, &ltx, ty, &lty,
           capt, &lcapt, &nws);
    return 0;
} /* main */

