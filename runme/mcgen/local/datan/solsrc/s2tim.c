#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__75 = 75;
static double c_d200 = 200., c_d100 = 100., c_dm20 = -20., c_dm1p5 = -1.5,
              c_d1p5 = 1.5, c_d0 = 0., c_d1 = 1., c_dp2 = .2, c_dp8 = .8,
              c_dmp314 = -.314, c_d1p1 = 1.1;

main(void)
{
    /* Local variables */
    static integer i, k, l, n, iswit, nws;
#if !defined(__TURBOC__)
    static
#endif
    double p, sigma, time, ymin, ymax, tt,
                  y[400], t[480], eta[480], etadel[480], coneta[480],
                  ata1[121] /* was [11][11] */,
                  ata1at[891] /* was [11][81] */,
                  scrat[121] /* was [11][11] */,
                  a[891] /* was [81][11] */;
    static char capt[75], string[1];
    /* System generated locals */
    integer i__1;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program S2TIM simulates data with errors corres-");
    printf("%s\n", "ponding to continuous or discontinuous functions,");
    printf("%s\n", "performs time series analysis on them");
    printf("%s\n\n", "and presents results graphically.");
/* ask for input */
    printf("%s\n", "Enter N(<= 400):");
    printf("%s", "> ");
    scanf("%li", &n);
    if (n > 400) n = 400;
    printf("%s\n", "Enter SIGMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter k ( 0 < k < min(40,N) ):");
    printf("%s", "> ");
    scanf("%li", &k);
    if (k > 40) k = 40;
    printf("%s\n", "Enter l ( 0 < l < k  and l < 10):");
    printf("%s", "> ");
    scanf("%li", &l);
    if (l > min(k,9)) l = min(k,9);
    printf("%s\n", "Enter P ( 0 < P < 1.0 ):");
    printf("%s", "> ");
    scanf("%lf", &p);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - sine function");
    printf("%s\n", "2 - step function");
    printf("%s\n", "3 - saw tooth function");
    printf("%s", "> ");
    scanf("%li", &iswit);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* generate time series */
    rnstnr_(y, &n);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        y[i - 1] *= sigma;
        time = (double) i;
        if (iswit == 1) {
            y[i - 1] = sin(time * .0174532f) + y[i - 1];
        } else if (iswit == 2) {
            tt = fmod(time, c_d200);
            if (tt <= 100.) {
                y[i - 1] += -1.;
            } else {
                y[i - 1] += 1.;
            }
        } else if (iswit == 3) {
            tt = fmod(time, c_d100);
            y[i - 1] = (tt - 50.) * .02f + y[i - 1];
        }
    }
    timser_(y, &n, &k, &l, &p, eta, coneta, a, ata1, ata1at, scrat);
    gropen_();
    gropws_(&nws);
    ymin = y[0];
    ymax = y[0];
    i__1 = n;
    for (i = 2; i <= i__1; ++i) {
/* Computing MIN */
        d__1 = y[i - 1];
        ymin = min(d__1,ymin);
/* Computing MAX */
        d__1 = y[i - 1];
        ymax = max(d__1,ymax);
    }
    d__1 = (double) (n + 20);
    grwncc_(&c_dm20, &d__1, &c_dm1p5, &c_d1p5);
    grvwwc_(&c_d0, &c_d1, &c_dp2, &c_dp8);
    grwnwc_(&c_dmp314, &c_d1p1, &c_d0, &c_d1);
    grboun_();
    grfram_();
/* write caption */
    sprintf(capt, "N = %3li, k = %3li, l = %3li, P = %5.3lf", n, k, l, p);
    memset(capt + 36, ' ', 39);
    grtxtc_(&c_d1, capt, &c__75);
    *(unsigned char *)string = 't';
    grsclx_(string, &c__1);
    *(unsigned char *)string = 'y';
    grscly_(string, &c__1);
/* draw data points */
    grstcl_(&c__2);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
        d__1 = (double) i;
        grdatp_(&c__1, &c_dp2, &d__1, &y[i - 1], &c_d0, &c_d0, &c_d0);
    }
/* draw moving averages */
    grstcl_(&c__3);
    i__1 = n + (k << 1);
    for (i = 1; i <= i__1; ++i) {
        t[i - 1] = (double) (i - k);
    }
    i__1 = n + (k << 1);
    grplin_(&i__1, t, eta);
    grstcl_(&c__4);
/* draw confidence limits */
    i__1 = n + (k << 1);
    for (i = 1; i <= i__1; ++i) {
        etadel[i - 1] = eta[i - 1] + coneta[i - 1];
    }
    i__1 = n + (k << 1);
    grplin_(&i__1, t, etadel);
    i__1 = n + (k << 1);
    for (i = 1; i <= i__1; ++i) {
        etadel[i - 1] = eta[i - 1] - coneta[i - 1];
    }
    i__1 = n + (k << 1);
    grplin_(&i__1, t, etadel);
    grclse_();
    return 0;
} /* main */

