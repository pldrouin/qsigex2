#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern void LIBFUNC rnbw(double const P_T *a, double const P_T *gamma,
                         double P_T *r, integer const P_T *n);
extern double CBFUNC minblp(double const P_T *x, integer const P_T *n);
extern double CBFUNC minbsq(double const P_T *x, integer const P_T *n);

/* Common Block Declarations */
/* COMMON /COM/ is needed for Microsoft FORTRAN compiler */
struct {
    double x[2], random[1000], xpl[1000], ypl[1000];
    integer list[2];
    double cx[4] /* was [2][2] */, dxplus[2], dxmins[2],
           scrat[6] /* was [2][3] */, scrat1[4] /* was [2][2] */;
} com;

struct cminbh {
    double t[100], hist[100], deltat;
    integer nt, nevent;
} LIBDATA cminbh;

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__74 = 74,
               c__1000 = 1000;
static double c_dm10p25 = -10.25, c_dp2 = .2, c_dp9 = .9, c_dp15 = .15,
              c_dp85 = .85, c_dmp414 = -.414, c_d0 = 0., c_dp5 = .5, c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, ich, ipl, ltx, lty, nws, iswit, nseed1, nseed2, lcapt,
                   nstep, nx, ny;
    static double dpl, dx1, dx2, epsiln, fact, fcont, fmin, gamma, rh, dx, dy,
                  xmin, xmax, ymin, ymax, a;
    static char capt[75], tx[75], ty[75];
    static logical ok;
    /* System generated locals */
    integer i__1;
    double d__1, d__2;
/* identify program to user */
    printf("%s\n", "Program S3MIN performs fit of Breit-Wigner function ");
    printf("%s\n\n", "to histogram by minimization.");
/* ask for input */
    printf("%s\n", "Enter number n of events (3 <= n <= 1000):");
    printf("%s", "> ");
    scanf("%li", &cminbh.nevent);
    printf("%s\n", "Enter number (>= 3, <= 100) of histogram bins:");
    printf("%s", "> ");
    scanf("%li", &cminbh.nt);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - likelihood minimization with Poisson stat.");
    printf("%s\n", "2 - minimization of sum of squares");
    printf("%s", "> ");
    scanf("%li", &ich);
/* draw sample */
    nseed1 = 87654;
    nseed2 = 98765;
    rne2in_(&nseed1, &nseed2);
    a = 0.;
    gamma = 1.;
    rnbw(&a, &gamma, com.random, &cminbh.nevent);
/* prepare and fill histogram */
    cminbh.deltat = 20.5 / (double) cminbh.nt;
    i__1 = cminbh.nt;
    for (i = 1; i <= i__1; ++i) {
        cminbh.t[i - 1] = (i - .5) * cminbh.deltat - 10.25;
    }
    smhsin_(cminbh.hist, &c_dm10p25, &cminbh.deltat, &cminbh.nt);
    i__1 = cminbh.nevent;
    for (i = 1; i <= i__1; ++i) {
        smhsfl_(cminbh.hist, &c_dm10p25, &cminbh.deltat, &cminbh.nt,
               &com.random[i - 1], &c_d1);
    }
/* set first approximation */
    com.x[0] = 1.;
    com.x[1] = 2.;
/* minimize with MINSIM */
    com.list[0] = 1;
    com.list[1] = 1;
    nstep = 0;
    epsiln = 0.;
    printf("%s\n", "minimization with MINSIM");
    printf("%s%4li%s%4li%s%4li%s%2li%2li\n", "N = ", cminbh.nt,
           ", NR = ", c__2, ", NRED = ", c__2, ", LIST = ",
           com.list[0], com.list[1]);
    printf("%s%10.5lf%10.5lf\n", "first approx.: X = ", com.x[0], com.x[1]);
    if (ich == 1) {
        minsim_(com.x, &c__2, &c__2, com.list, minblp, &fmin, &epsiln,
               &nstep, com.scrat);
    } else {
        minsim_(com.x, &c__2, &c__2, com.list, minbsq, &fmin, &epsiln,
               &nstep, com.scrat);
    }
    if (nstep < 0) {
        printf("%s\n", "minimization procedure failed");
        exit(0);
    }
    printf("%s%8.2lf%s%6li\n%s%10.5lf%10.5lf\n",
           "result of minimization: FMIN = ", fmin, ", NSTEP =", nstep,
           "X =", com.x[0], com.x[1]);
/* determine covariance matrix */
    fact = 1.;
    if (ich == 1) {
        mincov_(com.x, &c__2, &c__2, com.list, minblp, &fact,
               com.scrat, com.scrat1, com.cx, &ok);
    } else {
        mincov_(com.x, &c__2, &c__2, com.list, minbsq, &fact,
               com.scrat, com.scrat1, com.cx, &ok);
    }
    if (! ok) {
        printf("%s\n", "determination of covariance matrix fails");
        exit(0);
    }
    printf("%s\n", "covariance matrix CX = ");
    mtxwrt_(com.cx, &c__2, &c__2);
    printf("\n");
/* determine asymmetric errors */
    nstep = 0;
    fcont = fmin + .5;
    if (ich == 1) {
        minasy_(minblp, &c__2, &c__2, com.list, com.x, com.cx,
               &fcont, com.dxplus, com.dxmins, com.scrat, &nstep);
    } else {
        minasy_(minbsq, &c__2, &c__2, com.list, com.x, com.cx,
               &fcont, com.dxplus, com.dxmins, com.scrat, &nstep);
    }
    printf("%s\n", "asymmetric errors:");
    printf("%s%10.5lf%10.5lf\n", "DXPLUS = ", com.dxplus[0], com.dxplus[1]);
    printf("%s%10.5lf%10.5lf\n", "DXMINS = ", com.dxmins[0], com.dxmins[1]);
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
/* plot */
/* prepare caption */
    sprintf(capt, "N = %4li, x_1# = %6.3lf, x_2# = %6.3lf"
            ", &D@x_1# = %6.3lf, &D@x_2# = %6.3lf", cminbh.nevent,
            com.x[0], com.x[1], sqrt(com.cx[0]), sqrt(com.cx[3]));
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
        fact = (double) cminbh.nevent * cminbh.deltat * .6366198;
        dpl = .020520520520520519;
        for (ipl = 1; ipl <= 1000; ++ipl) {
            com.xpl[ipl - 1] = (double) (ipl - 1) * dpl - 10.25;
/* Computing 2nd power */
            d__1 = com.xpl[ipl - 1] - com.x[0];
/* Computing 2nd power */
            d__2 = com.x[1];
            com.ypl[ipl - 1] = fact * abs(com.x[1]) / (d__1 * d__1 * 4. +
                    d__2 * d__2);
        }
        grhscv_(com.xpl, com.ypl, &c__1000, cminbh.hist, &c_dm10p25,
               &cminbh.deltat, &cminbh.nt, tx, &ltx, ty, &lty, capt,
               &lcapt, &nws);
    } else {
/* plot confidence region */
        gropen_();
        gropws_(&nws);
        xmin = com.x[0] - sqrt(com.cx[0]) * 2.;
        xmax = com.x[0] * 2. - xmin;
        ymin = com.x[1] - sqrt(com.cx[3]) * 2.;
        ymax = com.x[1] * 2. - ymin;
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
        dx1 = sqrt(com.cx[0]);
        dx2 = sqrt(com.cx[3]);
        rh = com.cx[2] / (dx1 * dx2);
        if (abs(rh) < .001) {
            rh = .001;
        }
        grdatp_(&c__1, &c_dp5, com.x, &com.x[1], &dx1, &dx2, &rh);
/* draw asymmetric errors */
        grstcl_(&c__3);
        for (i = 1; i <= 2; ++i) {
            if (i == 1) {
                com.xpl[0] = com.x[0] - com.dxmins[0];
            } else {
                com.xpl[0] = com.x[0] + com.dxplus[0];
            }
            com.xpl[1] = com.xpl[0];
            com.ypl[0] = ymin;
            com.ypl[1] = ymax;
            grplin_(&c__2, com.xpl, com.ypl);
        }
        for (i = 1; i <= 2; ++i) {
            if (i == 1) {
                com.ypl[0] = com.x[1] - com.dxmins[1];
            } else {
                com.ypl[0] = com.x[1] + com.dxplus[1];
            }
            com.ypl[1] = com.ypl[0];
            com.xpl[0] = xmin;
            com.xpl[1] = xmax;
            grplin_(&c__2, com.xpl, com.ypl);
        }
        nx = 30;
        ny = 30;
        dx = (xmax - xmin) / nx;
        dy = (ymax - ymin) / ny;
        grstcl_(&c__4);
/* draw confidence region */
        if (ich == 1) {
            mincnt_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont,
                    com.x, &c__2, minblp);
        } else {
            mincnt_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont,
                    com.x, &c__2, minbsq);
        }
        grclse_();
    }
    return 0;
} /* main */

/* ------------------------------------------------------------------ */
void LIBFUNC rnbw(double const P_T *a, double const P_T *gamma,
                  double P_T *r, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;

    /* Parameter adjustments */
    --r;

    /* Function Body */
    rnecuy_(&r[1], n);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        r[i] = *a + *gamma * .5 * tan((r[i] - .5) * 3.14159);
    }
    return;
} /* rnbw */

/* ------------------------------------------------------------------ */
double CBFUNC minblp(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;
    double ret_val, d__1, d__2;

    /* Local variables */
    static integer i, ni;
    static double alam, alnlam, alnni, fnorm, gi;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    fnorm = (double) cminbh.nevent * cminbh.deltat;
    ret_val = 0.;
    i__1 = cminbh.nt;
    for (i = 1; i <= i__1; ++i) {
/* GI is the value of the probability density of the population at T(I) */
/* (by replacing the RHS of the following statement it can be */
/*  changed from normal to any desired distribution) */
/* Computing 2nd power */
        d__1 = cminbh.t[i - 1] - x[1];
/* Computing 2nd power */
        d__2 = x[2];
        gi = abs(x[2]) * .6366198 / (d__1 * d__1 * 4. + d__2 * d__2);
/* normalize to number of events in sample */
        gi = fnorm * gi;
        alam = gi;
        if (alam > 0.) {
            ni = nint(cminbh.hist[i - 1]);
            alnlam = log(alam);
            d__1 = (double) (ni + 1);
            alnni = glngam_(&d__1);
            ret_val = ret_val + alnni - cminbh.hist[i - 1] * alnlam + alam;
        }
    }
    return ret_val;
} /* minblp */

/* ------------------------------------------------------------------ */
double CBFUNC minbsq(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;
    double ret_val, d__1, d__2;

    /* Local variables */
    static integer i;
    static double fnorm, gi;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    fnorm = (double) cminbh.nevent * cminbh.deltat;
    ret_val = 0.;
    i__1 = cminbh.nt;
    for (i = 1; i <= i__1; ++i) {
/* GI is the value of the probability density of the population at T(I) */
/* (by replacing the RHS of the following statement it can be */
/*  changed from normal to any desired distribution) */
/* Computing 2nd power */
        d__1 = cminbh.t[i - 1] - x[1];
/* Computing 2nd power */
        d__2 = x[2];
        gi = abs(x[2]) * .6366198 / (d__1 * d__1 * 4. + d__2 * d__2);
/* normalize to number of events in sample */
        gi = fnorm * gi;
        if (cminbh.hist[i - 1] > 0.) {
/* Computing 2nd power */
            d__1 = cminbh.hist[i - 1] - gi;
            ret_val += d__1 * d__1 / cminbh.hist[i - 1];
        }
    }
    return ret_val;
} /* minbsq */

