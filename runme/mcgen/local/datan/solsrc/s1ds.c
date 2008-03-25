#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static double c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, n, iexp, nexp, lcapt, nh, ltx, lty, nws, npl, ipt;
    static double delh, fact, hist[120], b, r[1000], x, h0, xpl[121], ypl[121];
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1, i__2;
/* identify program to user */
    printf("%s\n", "Program S1DS performs Monte Carlo folding");
    printf("%s\n\n", "of uniform distributions.");
    printf("%s\n", "Enter number NEXP of experiments (>>1):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter number N of random numbers per experiment (>0):");
    printf("%s", "> ");
    scanf("%li", &n);
/* initialize histogram */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    strncpy(tx, "x", 1);
    strncpy(ty, "N(x)", 4);
    ltx = 1;
    lty = 4;
    sprintf(capt, "n_exp#=%5li, n=%5li", nexp, n);
/*    capt[21] = ' '; */
    lcapt = 25;
    h0 = -3.;
    delh = .05;
    nh = 120;
    smhsin_(hist, &h0, &delh, &nh);
    b = sqrt(3. / (double) n);
/* loop over all experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
/* simulate sample R */
        rnecuy_(r, &n);
        x = 0.;
        i__2 = n;
        for (i = 1; i <= i__2; ++i) {
            x += b * (r[i - 1] * 2. - 1.);
        }
/* enter chi squared into histogram */
        smhsfl_(hist, &h0, &delh, &nh, &x, &c_d1);
    }
/* curve of normal distribution */
    npl = nh + 1;
    fact = (double) nexp * delh;
    i__1 = npl;
    for (ipt = 1; ipt <= i__1; ++ipt) {
        xpl[ipt - 1] = h0 + (ipt - 1) * delh;
        ypl[ipt - 1] = fact * sdstnr_(&xpl[ipt - 1]);
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

