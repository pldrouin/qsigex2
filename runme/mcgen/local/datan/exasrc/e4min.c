#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Common Block Declarations */

struct cmingh_ {
    double t[100], hist[100], deltat;
    integer nt, nevent;
} LIBDATA cmingh_;

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__74 = 74;
static double c_dm5p25 = -5.25, c_d1 = 1., c_dp2 = .2, c_dp9 = .9, c_dp15 = .15,
              c_dp85 = .85, c_dmp414 = -.414, c_d0 = 0., c_dp5 = .5;

main(void)
{
    /* Local variables */
    static integer i, list[2], lcapt, nstep, iswit, nseed1, nseed2, nx, ny,
                   ich, ipl, npl, ltx, lty, nws;
    static double fact, fmin, xmin, xmax, ymin, ymax, x[2], fcont,
                  scrat[6] /* was [2][3] */, scrat1[4] /* was [2][2] */,
                  cx[4] /* was [2][2] */, rh, dx, dy, random[1000], epsiln,
                  dxmins[2], dx1, dx2, dxplus[2], dpl, xpl[100], ypl[100];
    static char capt[75], tx[75], ty[75];
    static logical ok;
    /* System generated locals */
    integer i__1;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program E4MIN performs fit of Gaussian");
    printf("%s\n\n", "to histogram by minimization.");
/* ask for input */
    printf("%s\n", "Enter number of events (>1,<=1000):");
    printf("%s", "> ");
    scanf("%li", &cmingh_.nevent);
    printf("%s\n", "Enter number of histogram bins (<101):");
    printf("%s", "> ");
    scanf("%li", &cmingh_.nt);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - likelihood minimization with Poisson stat.");
    printf("%s\n", "2 - minimization of sum of squares");
    printf("%s", "> ");
    scanf("%li", &ich);
/* draw sample */
    nseed1 = 1;
    nseed2 = 2;
    rne2in_(&nseed1, &nseed2);
    rnstnr_(random, &cmingh_.nevent);
/* prepare and fill histogram */
    cmingh_.deltat = 10.5 / (double) cmingh_.nt;
    i__1 = cmingh_.nt;
    for (i = 1; i <= i__1; ++i) {
        cmingh_.t[i - 1] = (i - .5) * cmingh_.deltat - 5.25;
    }
    smhsin_(cmingh_.hist, &c_dm5p25, &cmingh_.deltat, &cmingh_.nt);
    i__1 = cmingh_.nevent;
    for (i = 1; i <= i__1; ++i) {
        smhsfl_(cmingh_.hist, &c_dm5p25, &cmingh_.deltat, &cmingh_.nt,
               &random[i - 1], &c_d1);
    }
/* set first approximation */
    x[0] = 1.;
    x[1] = 2.;
/* minimize with MINSIM */
    list[0] = 1;
    list[1] = 1;
    nstep = 0;
    epsiln = 0.;
    printf("%s\n", "minimization with MINSIM");
    printf("%s%2li%s%2li%s%2li%s%2li%2li\n", "N = ", cmingh_.nt,
            ", NR = ", c__2, ", NRED = ", c__2, ", LIST = ", list[0], list[1]);
    printf("%s%10.5lf%10.5lf\n", "first approx.: X = ", x[0], x[1]);
    if (ich == 1) {
        minsim_(x, &c__2, &c__2, list, minglp_, &fmin, &epsiln, &nstep, scrat);
    } else {
        minsim_(x, &c__2, &c__2, list, mingsq_, &fmin, &epsiln, &nstep, scrat);
    }
    if (nstep < 0) {
        printf("%s\n", "minimization procedure failed");
        exit(0);
    }
    printf("%s%6.2lf%s%6li\n%s%10.5lf%10.5lf\n",
           "result of minimization: FMIN = ", fmin, ", NSTEP =", nstep,
           "X =", x[0], x[1]);
/* determine covariance matrix */
    fact = 1.;
    if (ich == 1) {
        mincov_(x, &c__2, &c__2, list, minglp_, &fact, scrat, scrat1, cx, &ok);
    } else {
        mincov_(x, &c__2, &c__2, list, mingsq_, &fact, scrat, scrat1, cx, &ok);
    }
    if (! ok) {
        printf("%s\n", "determination of covariance matrix fails");
        exit(0);
    }
    printf("%s\n", "covariance matrix CX = ");
    mtxwrt_(cx, &c__2, &c__2);
/* determine asymmetric errors */
    nstep = 0;
    fcont = fmin + .5;
    if (ich == 1) {
        minasy_(minglp_, &c__2, &c__2, list, x, cx, &fcont, dxplus, dxmins,
               scrat, &nstep);
    } else {
        minasy_(mingsq_, &c__2, &c__2, list, x, cx, &fcont, dxplus, dxmins,
               scrat, &nstep);
    }
    printf("\n%s\n", "asymmetric errors:");
    printf("%s%10.5lf%10.5lf\n", "DXPLUS = ", dxplus[0], dxplus[1]);
    printf("%s%10.5lf%10.5lf\n", "DXMINS = ", dxmins[0], dxmins[1]);
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
/* plot */
/* prepare caption */
    sprintf(capt, "N = %4li, x_1# = %6.3lf, x_2# = %6.3lf"
            ", &D@x_1# = %6.3lf, &D@x_2# = %6.3lf", cmingh_.nt, x[0], x[1],
            sqrt(cx[0]), sqrt(cx[3]));
    lcapt = strlen(capt);
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    strncpy(tx, "y", 1);
    ltx = 1;
    strncpy(ty, "N(y)", 4);
    lty = 4;
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - plot of histogram and fitted function");
    printf("%s\n", "2 - plot of parameters and confidence region");
    printf("%s", "> ");
    scanf("%li", &iswit);
    (void)getchar();
    if (iswit == 1) {
/* draw Gaussian corresponding to solution */
        fact = (double) cmingh_.nevent * cmingh_.deltat * .3989423 / x[1];
        npl = 100;
        dpl = 10.5 / (double) (npl - 1);
        i__1 = npl;
        for (ipl = 1; ipl <= i__1; ++ipl) {
            xpl[ipl - 1] = (double) (ipl - 1) * dpl - 5.25;
/* Computing 2nd power */
            d__1 = (xpl[ipl - 1] - x[0]) / x[1];
            ypl[ipl - 1] = fact * exp(d__1 * d__1 * -.5);
        }
        grhscv_(xpl, ypl, &npl, cmingh_.hist, &c_dm5p25, &cmingh_.deltat,
               &cmingh_.nt, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
    } else {
/* plot confidence region */
        gropen_();
        gropws_(&nws);
        xmin = x[0] - sqrt(cx[0]) * 2.;
        xmax = x[0] * 2. - xmin;
        ymin = x[1] - sqrt(cx[3]) * 2.;
        ymax = x[1] * 2. - ymin;
        grwncc_(&xmin, &xmax, &ymin, &ymax);
        grvwwc_(&c_dp2, &c_dp9, &c_dp15, &c_dp85);
        grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
        grfram_();
        grboun_();
        grtxtc_(&c_d1, capt, &c__74);
        strncpy(tx, "x_1", 3);
        grsclx_(tx, &c__3);
        strncpy(ty, "x_2", 3);
        ty[3] = ' ';
        grscly_(ty, &c__3);
/* draw solution with symmetric errors and covariance ellipse */
        grstcl_(&c__2);
        dx1 = sqrt(cx[0]);
        dx2 = sqrt(cx[3]);
        rh = cx[2] / (dx1 * dx2);
        if (abs(rh) < .001) {
            rh = .001;
        }
        grdatp_(&c__1, &c_dp5, x, &x[1], &dx1, &dx2, &rh);
/* draw asymmetric errors */
        grstcl_(&c__3);
        for (i = 1; i <= 2; ++i) {
            if (i == 1) {
                xpl[0] = x[0] - dxmins[0];
            } else {
                xpl[0] = x[0] + dxplus[0];
            }
            xpl[1] = xpl[0];
            ypl[0] = ymin;
            ypl[1] = ymax;
            grplin_(&c__2, xpl, ypl);
        }
        for (i = 1; i <= 2; ++i) {
            if (i == 1) {
                ypl[0] = x[1] - dxmins[1];
            } else {
                ypl[0] = x[1] + dxplus[1];
            }
            ypl[1] = ypl[0];
            xpl[0] = xmin;
            xpl[1] = xmax;
            grplin_(&c__2, xpl, ypl);
        }
        nx = 30;
        ny = 30;
        dx = (xmax - xmin) / nx;
        dy = (ymax - ymin) / ny;
        grstcl_(&c__4);
/* draw confidence region */
        if (ich == 1) {
            mincnt_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont, x,
                   &c__2, minglp_);
        } else {
            mincnt_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont, x,
                   &c__2, mingsq_);
        }
        grclse_();
    }
    return 0;
} /* main */

