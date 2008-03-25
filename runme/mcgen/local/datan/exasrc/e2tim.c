#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__75 = 75;
static double c_dm10 = -10., c_d50 = 50., c_dm20 = -20., c_d70 = 70., c_d0 = 0., 
              c_d1 = 1., c_dp2 = .2, c_dp8 = .8, c_dmp314 = -.314, c_d1p1 = 1.1;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer i, k, l, n, nws;
    static double y[36] = { 38.7,50.3,45.6,46.4,43.7,42.,21.8,21.8,51.3,
	      39.5,26.9,23.2,19.8,24.4,17.1,29.3,43.,35.9,19.6,33.2,38.8,35.3,
	      23.4,14.9,15.3,17.7,16.5,6.6,9.5,9.1,3.1,9.3,4.7,6.1,7.4,15.1 },
           ata1at[126] /* was [6][21] */, etadel[50], coneta[50],
           a[126] /* was [21][6] */, p, t[50], scrat[36] /* was [6][6] */,
           eta[50], ata1[36] /* was [6][6] */;
    static char capt[75], string[1];
    /* System generated locals */
    integer i__1;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program E2TIM performs time series analysis");
    printf("%s\n\n", "and presents results graphically.");
/* ask for input */
    printf("%s\n", "Enter k ( 0 < k < 5 ):");
    printf("%s", "> ");
    scanf("%li", &k);
    printf("%s\n", "Enter l ( 0 < l < k ):");
    printf("%s", "> ");
    scanf("%li", &l);
    printf("%s\n", "Enter P ( 0. < P < 1.0 ):");
    printf("%s", "> ");
    scanf("%lf", &p);
    n = 36;
    timser_(y, &n, &k, &l, &p, eta, coneta, a, ata1, ata1at, scrat);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
    gropen_();
    gropws_(&nws);
    grwncc_(&c_dm10, &c_d50, &c_dm20, &c_d70);
    grvwwc_(&c_d0, &c_d1, &c_dp2, &c_dp8);
    grwnwc_(&c_dmp314, &c_d1p1, &c_d0, &c_d1);
    grboun_();
    grfram_();
/* write caption */
    sprintf(capt, "N = %3li, k = %3li, l = %3li, P = %5.3lf", n, k, l, p);
    memset(capt + 36, ' ', 39);
    grtxtc_(&c_d1, capt, &c__75);
    *string = 't';
    grsclx_(string, &c__1);
    *string = 'y';
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

