#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__9 = 9;
static double c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, k, iexp, lcapt, ltx, lty, nws, nexp, nh, npl, nr;
    static double a[27] /* was [9][3] */, cx[9] /* was [3][3] */, delh, fact,
                  deltay[9], hist[100], p, r, t[9], x[3], y[9], sigmaf, sigmag,
                  scrat[9] /* was [3][3] */, h0, h1, xin[3], xpl[2], ypl[2];
    static char capt[75], tx[75], ty[75];
    static logical ok;
    /* System generated locals */
    integer i__1, i__2, i__3;
/* identify program to user */
    printf("%s\n", "Program S1LSQ generates data");
    printf("%s\n", "corresponding to 2nd degree polynomial");
    printf("%s\n\n", "and fits 1st degree polynomial.");
/* ask for input */
    printf("%s\n", "Enter number NEXP of experiments (>>1):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter X1 for points to be generated:");
    printf("%s", "> ");
    scanf("%lf", &xin[0]);
    printf("%s\n", "Enter X2 for points to be generated:");
    printf("%s", "> ");
    scanf("%lf", &xin[1]);
    printf("%s\n", "Enter X3 for points to be generated:");
    printf("%s", "> ");
    scanf("%lf", &xin[2]);
    printf("%s\n", "Enter SIGMA for points to be generated (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigmag);
    printf("%s\n", "Enter DELTAY to be used in fit (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigmaf);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* initialize histogram and prepare graphics */
    h1 = 1.;
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    strncpy(tx, "P(&c@^2#)", 9);
    strncpy(ty, "N(P)", 4);
    ltx = 9;
    lty = 4;
    sprintf(capt, "Input: x_1#=%6.2lf, x_2#=%6.2lf, x_3#=%6.2lf,"
            " &s=%6.2lf, &D@y=%6.2lf", xin[0], xin[1], xin[2], sigmag, sigmaf);
    lcapt = strlen(capt);
    h0 = 0.;
    delh = h1 * .01;
    nh = 100;
/* curve of constant probability */
    fact = (h1 - h0) * (double) nexp / (double) nh;
    xpl[0] = h0;
    xpl[1] = h1;
    ypl[0] = fact;
    ypl[1] = ypl[0];
    npl = 2;
    smhsin_(hist, &h0, &delh, &nh);
/* loop over all simulated experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
/* generate data points corresponding to polynomial */
        nr = 3;
        rnstnr_(deltay, &c__9);
        for (i = 1; i <= 9; ++i) {
            t[i - 1] = (double) (i - 1) * 1. - 4.;
            y[i - 1] = xin[0];
            if (nr > 1) {
                i__2 = nr;
                for (k = 2; k <= i__2; ++k) {
                    i__3 = k - 1;
                    y[i - 1] += xin[k - 1] * pow(t[i - 1], (double)i__3);
                }
            }
            y[i - 1] += deltay[i - 1] * sigmag;
            deltay[i - 1] = sigmaf;
        }
/* perform polynomial fit */
        nr = 2;
        lsqpol_(t, y, deltay, &c__9, &nr, x, cx, &r, a, scrat, &ok);
/* compute chi-squared probability and enter it in histogram */
        i__2 = 9 - nr;
        p = 1. - scchi2_(&r, &i__2);
        smhsfl_(hist, &h0, &delh, &nh, &p, &c_d1);
    }
/* graphical output */
    grhscv_(xpl, ypl, &npl, hist, &h0, &delh, &nh, tx, &ltx, ty, &lty, capt,
           &lcapt, &nws);
    return 0;
} /* main */

