#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__5 = 5, c__9 = 9;
static double c_d1 = 1.;

main(void)
{
    /* Initialized data */
    static char tx[10] = "t^^\"-     ", ty[10] = "N(t^^\"-#) ",
                capt[10] = "          ";
    /* Local variables */
    static integer i, n, iexp, iswit, nexp, nws, nx;
    static double f, hist[100], t[1000], hlast, tbar, delx, x0;
    /* System generated locals */
    integer i__1, i__2;
/* Likelihood estimate for mean life in radioactive decay */
/* identify program to user */
    printf("%s\n", "Program S1ML plots likelihood estimate TBAR");
    printf("%s\n", "for mean life in radioactive decay");
    printf("%s\n", "determined in several experiments");
    printf("%s\n\n", "with few decays each.");
/* ask for input */
    printf("%s\n", "Enter number NEXP of experiments (>>1):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter number N of decays (1 <= N <= 1000):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - Histogram of TBAR");
    printf("%s\n", "2 - Cumulative frequency distribution of TBAR");
    printf("%s", "> ");
    scanf("%li", &iswit);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* initialize histogram */
    x0 = 0.;
    delx = .05;
    nx = 100;
    smhsin_(hist, &x0, &delx, &nx);
/* loop over all experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
/* generate sample and draw 1D scatter diagram */
        rnecuy_(t, &n);
        tbar = 0.f;
        i__2 = n;
        for (i = 1; i <= i__2; ++i) {
            t[i - 1] = -log(t[i - 1]);
            tbar += t[i - 1];
        }
        tbar /= (double) n;
        smhsfl_(hist, &x0, &delx, &nx, &tbar, &c_d1);
    }
    if (iswit == 1) {
        strncpy(ty, "N(t^^\"-#)", 9);
        smhsgr_(hist, &x0, &delx, &nx, tx, &c__5, ty, &c__9, capt, &c__1, &nws);
    } else {
        f = 1. / (double) nexp;
        hlast = 0.;
        i__1 = nx;
        for (i = 1; i <= i__1; ++i) {
            hlast += hist[i - 1] * f;
            hist[i - 1] = hlast;
        }
        strncpy(ty, "F(t^^\"-#)", 9);
        smhsgr_(hist, &x0, &delx, &nx, tx, &c__5, ty, &c__9, capt, &c__1, &nws);
    }
    return 0;
} /* main */

