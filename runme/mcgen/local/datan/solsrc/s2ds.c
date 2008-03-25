#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1;
static double c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer iexp, lcapt, ltx, lty, nws, nexp, nh, npl, ipt;
    static double arg1, arg2, delh, fact, h0, h1, rnorm, entry__, hist[100],
                  runi, a, b, sigma, xpl[101], ypl[101];
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S2DS performs Monte Carlo folding");
    printf("%s\n\n", "of uniform with Gaussian distribution.");
/* ask for parameters */
    printf("%s\n", "Enter number NEXP of experiments (>>1):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter left edge A of uniform distribution:");
    printf("%s", "> ");
    scanf("%lf", &a);
    printf("%s\n", "Enter right edge B of uniform distribution:");
    printf("%s", "> ");
    scanf("%lf", &b);
    printf("%s\n", "Enter width SIGMA of Gaussian distribution (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
/* initialize histogram */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    strncpy(tx, "x", 1);
    strncpy(ty, "N(x)", 4);
    ltx = 1;
    lty = 4;
    sprintf(capt, "n_exp#=%5li, a=%6.2lf, b=%6.2lf, &s=%6.2lf",
            nexp, a, b, sigma);
    lcapt = strlen(capt);
    h0 = a - sigma * 3.;
    h1 = b + sigma * 3.;
    delh = (h1 - h0) * .01;
    nh = 100;
    smhsin_(hist, &h0, &delh, &nh);
/* loop over all experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
        rnecuy_(&runi, &c__1);
        runi = a + (b - a) * runi;
        rnstnr_(&rnorm, &c__1);
        rnorm *= sigma;
        entry__ = runi + rnorm;
/* fill histogram histogram */
        smhsfl_(hist, &h0, &delh, &nh, &entry__, &c_d1);
    }
/* curve of folding between uniform and normal distribution */
    npl = nh + 1;
    fact = (double) nexp * delh / (b - a);
    i__1 = npl;
    for (ipt = 1; ipt <= i__1; ++ipt) {
        xpl[ipt - 1] = h0 + (ipt - 1) * delh;
        arg1 = (b - xpl[ipt - 1]) / sigma;
        arg2 = (a - xpl[ipt - 1]) / sigma;
        ypl[ipt - 1] = fact * (scstnr_(&arg1) - scstnr_(&arg2));
    }
/* graphical output */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
    grhscv_(xpl, ypl, &npl, hist, &h0, &delh, &nh, tx, &ltx, ty, &lty, capt,
           &lcapt, &nws);
    return 0;
} /* main */

