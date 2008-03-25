#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__75 = 75;
static double c_dm20 = -20., c_d0 = 0., c_d1 = 1., c_dp2 = .2, c_dp8 = .8, 
              c_dmp314 = -.314, c_d1p1 = 1.1;

struct {
    double ata1at[861] /* was [21][41] */, etadel[440], coneta[440],
           t[440], y[400], scrat[441] /* was [21][21] */,
           a[861] /* was [41][21] */, ata1[441] /* was [21][21] */, eta[440];
} LIBDATA s1tcom;

main(void)
{
    /* Local variables */
    static integer i, k, l, m, n, nws;
    static double p, x, sigma, time, ymin, ymax, d, f;
    static char capt[75], string[1];
    /* System generated locals */
    integer i__1;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program S1TIM simulates data with errors");
    printf("%s\n", "performs time series analysis on them");
    printf("%s\n\n", "and presents results graphically.");
/* ask for input */
    printf("%s\n", "Enter power m (>=0):");
    printf("%s", "> ");
    scanf("%li", &m);
    printf("%s\n", "Enter SIGMA (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Enter k ( 0 < k < 21 ):");
    printf("%s", "> ");
    scanf("%li", &k);
    printf("%s\n", "Enter l ( 0 < l < k ):");
    printf("%s", "> ");
    scanf("%li", &l);
    printf("%s\n", "Enter P ( 0 < P < 1.0 ):");
    printf("%s", "> ");
    scanf("%lf", &p);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* generate time series */
    n = 200;
    rnstnr_(s1tcom.y, &n);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	s1tcom.y[i - 1] *= sigma;
	time = (double) i;
	x = (time - 100.) * .02;
	f = pow(x, (double)m);
	s1tcom.y[i - 1] += f;
    }
    timser_(s1tcom.y, &n, &k, &l, &p, s1tcom.eta, s1tcom.coneta, s1tcom.a,
           s1tcom.ata1, s1tcom.ata1at, s1tcom.scrat);
    gropen_();
    gropws_(&nws);
    ymin = s1tcom.y[0];
    ymax = s1tcom.y[0];
    i__1 = n;
    for (i = 2; i <= i__1; ++i) {
/* Computing MIN */
	d__1 = s1tcom.y[i - 1];
	ymin = min(d__1,ymin);
/* Computing MAX */
	d__1 = s1tcom.y[i - 1];
	ymax = max(d__1,ymax);
    }
    d = ymax - ymin;
    ymin -= d * .2;
    ymax += d * .2;
    d__1 = (double) (n + 20);
    grwncc_(&c_dm20, &d__1, &ymin, &ymax);
    grvwwc_(&c_d0, &c_d1, &c_dp2, &c_dp8);
    grwnwc_(&c_dmp314, &c_d1p1, &c_d0, &c_d1);
    grboun_();
    grfram_();
/* write caption */
    memset(capt, ' ', 75);
    sprintf(capt, "N = %3li, k = %3li, l = %3li, P = %5.3lf", n, k, l, p);
    capt[36] = ' ';
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
	grdatp_(&c__1, &c_dp2, &d__1, &s1tcom.y[i - 1], &c_d0, &c_d0, &c_d0);
    }
/* draw moving averages */
    grstcl_(&c__3);
    i__1 = n + (k << 1);
    for (i = 1; i <= i__1; ++i) {
	s1tcom.t[i - 1] = (double) (i - k);
    }
    i__1 = n + (k << 1);
    grplin_(&i__1, s1tcom.t, s1tcom.eta);
    grstcl_(&c__4);
/* draw confidence limits */
    i__1 = n + (k << 1);
    for (i = 1; i <= i__1; ++i) {
	s1tcom.etadel[i - 1] = s1tcom.eta[i - 1] + s1tcom.coneta[i - 1];
    }
    i__1 = n + (k << 1);
    grplin_(&i__1, s1tcom.t, s1tcom.etadel);
    i__1 = n + (k << 1);
    for (i = 1; i <= i__1; ++i) {
	s1tcom.etadel[i - 1] = s1tcom.eta[i - 1] - s1tcom.coneta[i - 1];
    }
    i__1 = n + (k << 1);
    grplin_(&i__1, s1tcom.t, s1tcom.etadel);
    grclse_();
    return 0;
} /* main */

