#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Common Block Declarations */

struct cmings_ {
    double y[1000];
    integer nny;
} LIBDATA cmings_;

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__74 = 74;
static double c_dmp2 = -.2, c_dp9 = .9, c_dp15 = .15, c_dp85 = .85,
              c_dmp414 = -.414, c_d1 = 1., c_d0 = 0.;

main(void)
{
    /* System generated locals */
    integer i__1;
    double d__1;

    /* Local variables */
    static integer i, k, ipl, npl, list[2], nstep, nseed1, nseed2, nws;
    static double fact, fmin, xmin, xmax, ymin, ymax, x[2],
                  scrat[6] /* was [2][3] */, scrat1[4] /* was [2][2] */,
                  cx[4] /* was [2][2] */, epsiln, dpl, xpl[100], ypl[100];
    static char string[75];
    static logical ok;

/* identify program to user */
    printf("%s\n", "Program E2MIN demonstrates use of MINCOV");
    printf("%s\n", "by fitting Gaussian to small sample");
    printf("%s\n\n", "and determining errors of parameters by MINCOV.");
/* ask for input */
    printf("%s\n", "Enter number n of events (2 <= n <= 1000):");
    printf("%s", "> ");
    scanf("%li", &cmings_.nny);
/* draw sample */
    nseed1 = 1;
    nseed2 = 2;
    rne2in_(&nseed1, &nseed2);
    rnstnr_(cmings_.y, &cmings_.nny);
/* write table of data */
    printf("%s\n", "sample Y is");
    mtxwrt_(cmings_.y, &c__1, &cmings_.nny);
/* set first approximation */
    x[0] = 1.;
    x[1] = 2.;
/* minimize with MINSIM */
    list[0] = 1;
    list[1] = 1;
    nstep = 10000;
    epsiln = 0.;
    fmin = .01;
    printf("%s\n", "minimization with MINSIM");
    printf("%s%2li%s%2li%s%2li%s%2li%2li\n", "N = ", cmings_.nny,
            ", NR = ", c__2, ", NRED = ", c__2, ", LIST = ", list[0], list[1]);
    printf("%s%10.5lf%10.5lf\n", "first approx.: X = ", x[0], x[1]);
    minsim_(x, &c__2, &c__2, list, mingls_, &fmin, &epsiln, &nstep, scrat);
    printf("%s%6.2lf%s%6li\n%s%10.5lf%10.5lf\n",
          "result of minimization: FMIN = ", fmin, ", NSTEP =", nstep,
          "X =", x[0], x[1]);
/* determine covariance matrix */
    fact = 1.;
    mincov_(x, &c__2, &c__2, list, mingls_, &fact, scrat, scrat1, cx, &ok);
    if (! ok) {
        printf("%s\n", "determination of covariance matrix fails");
        exit(0);
    }
    printf("%s\n", "covariance matrix CX = ");
    mtxwrt_(cx, &c__2, &c__2);
/* ask for number of workstation */
    grnbws_();
    printf("\n%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* plot sample as one dimensional scatter plot and Gaussian */
    xmin = -5.;
    xmax = 5.;
    ymin = 0.;
    ymax = .5;
    gropen_();
    gropws_(&nws);
    grwncc_(&xmin, &xmax, &ymin, &ymax);
    grvwwc_(&c_dmp2, &c_dp9, &c_dp15, &c_dp85);
    grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
    grfram_();
    grboun_();
    sprintf(string, "N = %4li, x_1# = %6.3lf, x_2# = %6.3lf"
            ", &D@x_1# = %6.3lf, &D@x_2# = %6.3lf", cmings_.nny, x[0], x[1],
            sqrt(cx[0]), sqrt(cx[3]));
    string[74] = ' ';
    grtxtc_(&c_d1, string, &c__74);
    memset(string, ' ', 75);
    strncpy(string, "x_1", 3);
    grsclx_(string, &c__3);
    strncpy(string, "x_2", 3);
    grscly_(string, &c__3);
/* plot scatter diagram */
    grstcl_(&c__2);
    i__1 = cmings_.nny;
    for (i = 1; i <= i__1; ++i) {
        xpl[0] = cmings_.y[i - 1];
        xpl[1] = xpl[0];
        ypl[0] = 0.;
        ypl[1] = .1;
        grplin_(&c__2, xpl, ypl);
    }
/* draw Gaussian corresponding to solution */
    fact = .3989423 / x[1];
    npl = 100;
    dpl = (xmax - xmin) / (double) (npl - 1);
    i__1 = npl;
    for (ipl = 1; ipl <= i__1; ++ipl) {
        xpl[ipl - 1] = xmin + (double) (ipl - 1) * dpl;
/* Computing 2nd power */
        d__1 = (xpl[ipl - 1] - x[0]) / x[1];
        ypl[ipl - 1] = fact * exp(d__1 * d__1 * -.5);
    }
    grstcl_(&c__4);
    grplin_(&npl, xpl, ypl);
    grclse_();
    return 0;
} /* main */

