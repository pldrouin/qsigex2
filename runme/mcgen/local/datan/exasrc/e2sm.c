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
    static integer ltx, lty, nws, i, n, lcapt, nx;
    static double delx, hist[1000], x0, sample[1];
    static char capt[75], tx[75], ty[75];

/* identify program to user */
    printf("%s\n", "Program E1SM demonstrates the use of");
    printf("%s\n", "subroutines SMHSIN, SMHSFL, SMHSGR");
    printf("%s\n", "on a sample taken from the");
    printf("%s\n\n", "standardized normal distribution.");
/* ask for input */
    printf("%s\n", "Enter number N of elements in sample (>0):");
    printf("%s", "> ");
    scanf("%ld", &n);
    printf("%s\n", "Enter lower edge X0 of histogram:");
    printf("%s", "> ");
    scanf("%lf", &x0);
    printf("%s\n", "Enter bin width DELX of histogram:");
    printf("%s", "> ");
    scanf("%lf", &delx);
    printf("%s\n", "Enter number NX of histogram bins (<1001):");
    printf("%s", "> ");
    scanf("%ld", &nx);
/* define texts */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    strncpy(tx, "x", 1);
    ltx = 1;
    strncpy(ty, "N(x)", 4);
    lty = 4;
    strncpy(capt, "N = ******", 10);
    sprintf(&capt[4], "%6ld", n);
    lcapt = strlen(capt);
/* initialize histogram */
    smhsin_(hist, &x0, &delx, &nx);
/* fill histogram */
    for (i = 1; i <= n; ++i) {
        rnstnr_(sample, &c__1);
        smhsfl_(hist, &x0, &delx, &nx, sample, &c_d1);
    }
/* graphical output */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%ld", &nws);
    (void)getchar();
    smhsgr_(hist, &x0, &delx, &nx, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
    return 0;
} /* main */

