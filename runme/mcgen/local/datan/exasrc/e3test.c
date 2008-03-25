#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static double c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, n, iexp, iswit, nexp, nh, npl, ipt, lcapt, ltx, lty, nws;
    static double delh, fact, hist[100], a, p, r[1000], sigma, a0, h0, h1,
                  sigma0, xpl[101], ypl[101], chi2;
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1, i__2;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program E3TEST simulates set of measurements");
    printf("%s\n\n", "and performs chi-squared test.");
/* ask for input */
    printf("%s\n", "Enter number NEXP of experiments (>0):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter number N of measurements per experiment (>0):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter mean A for generation:");
    printf("%s", "> ");
    scanf("%lf", &a);
    printf("%s\n", "Enter hypothesis A0 for mean:");
    printf("%s", "> ");
    scanf("%lf", &a0);
    printf("%s\n", "Enter standard deviation SIGMA for generation (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter hypothesis SIGMA0 for standard deviation (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma0);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - histogram of chi squared");
    printf("%s\n", "2 - histogram of chi-squared probability");
    printf("%s", "> ");
    scanf("%li", &iswit);
/* initialize histogram */
    sprintf(capt, "n_exp#=%5li, n=%5li, a=%6.2lf, "
            "&s@=%6.2lf, a_0#=%6.2lf, &s@_0#=%6.2lf",
            nexp, n, a, sigma, a0, sigma0);
    lcapt = strlen(capt);
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    if (iswit == 1) {
        h1 = (double) (n << 2);
        strncpy(tx, "&c@^2", 5);
        strncpy(ty, "N(&c@^2#)", 9);
        ltx = 5;
        lty = 9;
    } else {
        h1 = 1.;
        strncpy(tx, "P(&c@^2#)", 9);
        strncpy(ty, "N(P)", 4);
        ltx = 9;
        lty = 4;
    }
    h0 = 0.;
    delh = h1 * .01;
    nh = 100;
    smhsin_(hist, &h0, &delh, &nh);
/* loop over all experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
/* simulate sample R */
        rnstnr_(r, &n);
        i__2 = n;
        for (i = 1; i <= i__2; ++i) {
            r[i - 1] = a + r[i - 1] * sigma;
        }
/* compute chi squared */
        chi2 = 0.;
        i__2 = n;
        for (i = 1; i <= i__2; ++i) {
/* Computing 2nd power */
            d__1 = (r[i - 1] - a0) / sigma0;
            chi2 += d__1 * d__1;
        }
        if (iswit == 1) {
/* enter chi squared into histogram */
            smhsfl_(hist, &h0, &delh, &nh, &chi2, &c_d1);
        } else {
/* compute probability and enter it into histogram */
            p = 1. - scchi2_(&chi2, &n);
            smhsfl_(hist, &h0, &delh, &nh, &p, &c_d1);
        }
    }
    fact = (h1 - h0) * (double) nexp / (double) nh;
    if (iswit == 1) {
/* curve of chi-squared distribution */
        npl = nh + 1;
        i__1 = npl;
        for (ipt = 1; ipt <= i__1; ++ipt) {
            xpl[ipt - 1] = h0 + (ipt - 1) * delh;
            if (ipt == 1 && n == 1) {
                xpl[ipt - 1] = 1e-5;
            }
            ypl[ipt - 1] = fact * sdchi2_(&xpl[ipt - 1], &n);
        }
    } else {
/* curve of constant probability */
        xpl[0] = h0;
        xpl[1] = h1;
        ypl[0] = fact;
        ypl[1] = ypl[0];
        npl = 2;
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

