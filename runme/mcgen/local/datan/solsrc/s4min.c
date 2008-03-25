#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern void LIBFUNC rnbw(double const P_T *a, double const P_T *gamma,
                         double P_T *r, integer const P_T *n);
extern double LIBFUNC minblp(double const P_T *x, integer const P_T *n);
extern double LIBFUNC minbsq(double const P_T *x, integer const P_T *n);
extern double LIBFUNC brewig(double const P_T *x, double const P_T *a,
                             double const P_T *gamma);

/* Common Block Declarations */
/* COMMON /COM/ is needed for Microsoft FORTRAN compiler */
struct {
    double x[5], random[1000], xpl[1000], ypl[1000], cx[25] /* was [5][5] */,
           scrat[30] /* was [5][6] */, xmc[5], scrat1[25] /* was [5][5] */;
    integer list[5];
} com;

struct cminbh {
    double t[100], hist[100], deltat;
    integer nt, nevent;
} LIBDATA cminbh;

/* Table of constant values */

static integer c__1 = 1, c__5 = 5, c__1000 = 1000;
static double c_dm10p25 = -10.25, c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, lcapt, nstep, nseed1, nseed2, ich, ipl, ltx, lty, nws,
                   nred;
    static double dpl, epsiln, fact, fmin;
    static char capt[75], tx[75], ty[75], *text[5] =
                { "fraction of events in 1st BW function",
                  "mean of 1st BW function (>= -5., <= 5.)",
                  "FWHM of 1st BW function  (>0.)",
                  "mean of 2nd BW function (>= -5., <= 5.)",
                  "FWHM of 2nd BW function  (>0.)" };
    static logical ok;
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S4MIN performs fit of 2 Breit-Wigner");
    printf("%s\n", "functions to sample or histogram");
    printf("%s\n\n", "by minimization.");
/* ask for input */
    printf("%s\n", "Enter number of events (<= 1000):");
    printf("%s", "> ");
    scanf("%li", &cminbh.nevent);
    printf("%s\n", "Enter number of histogram bins (<= 100):");
    printf("%s", "> ");
    scanf("%li", &cminbh.nt);
    for (i = 1; i <= 5; ++i) {
        printf("%s%s:\n", "Enter ", text[i-1]);
        printf("%s", "> ");
        scanf("%lf", &com.xmc[i-1]);
        printf("%s\n", "Enter initial value for that parameter:");
        printf("%s", "> ");
        scanf("%lf", &com.x[i-1]);
        printf("%s\n", "Enter:");
        printf("%s\n", "1 - if parameter is to be kept variable");
        printf("%s\n", "0 - if parameter is to fixed");
        printf("%s", "> ");
        scanf("%li", &com.list[i-1]);
    }
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - likelihood minimization with Poisson stat.");
    printf("%s\n", "2 - minimization of sum of squares");
    printf("%s", "> ");
    scanf("%li", &ich);
/* draw sample */
    nseed1 = 87654;
    nseed2 = 98765;
    rne2in_(&nseed1, &nseed2);
    rnecuy_(com.random, &cminbh.nevent);
    i__1 = cminbh.nevent;
    for (i = 1; i <= i__1; ++i) {
        if (com.random[i - 1] < com.xmc[0]) {
            rnbw(&com.xmc[1], &com.xmc[2], &com.random[i - 1], &c__1);
        } else {
            rnbw(&com.xmc[3], &com.xmc[4], &com.random[i - 1], &c__1);
        }
    }
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
/* set NRED */
    nred = 0;
    for (i = 1; i <= 5; ++i) {
        if (com.list[i - 1] == 1) {
            ++nred;
        }
    }
/* minimize with MINSIM */
    nstep = 0;
    epsiln = 0.;
    printf("%s\n", "minimization with MINSIM");
    printf("%s%4li%s%4li%s%4li%s%2li%2li%2li%2li%2li\n", "N = ", cminbh.nt,
           ", NR = ", c__5, ", NRED = ", nred, ", LIST = ",
           com.list[0], com.list[1], com.list[2], com.list[3], com.list[4]);
    printf("%s%10.5lf%10.5lf%10.5lf%10.5lf%10.5lf\n", "first approx.: X = ",
           com.x[0], com.x[1], com.x[2], com.x[3], com.x[4]);
    if (ich == 1) {
        minsim_(com.x, &c__5, &nred, com.list, minblp, &fmin,
                &epsiln, &nstep, com.scrat);
    } else {
        minsim_(com.x, &c__5, &nred, com.list, minbsq, &fmin,
               &epsiln, &nstep, com.scrat);
    }
    if (nstep < 0) {
        printf("%s\n", "minimization procedure failed");
        exit(0);
    }
    printf("%s%8.2lf%s%6li\n%s%10.5lf%10.5lf%10.5lf%10.5lf%10.5lf\n",
           "result of minimization: FMIN = ", fmin, ", NSTEP =", nstep,
           "X =", com.x[0], com.x[1], com.x[2], com.x[3], com.x[4]);
/* determine covariance matrix */
    fact = 1.;
    if (ich == 1) {
        mincov_(com.x, &c__5, &nred, com.list, minblp, &fact,
                com.scrat, com.scrat1, com.cx, &ok);
    } else {
        mincov_(com.x, &c__5, &nred, com.list, minbsq, &fact,
                com.scrat, com.scrat1, com.cx, &ok);
    }
    if (! ok) {
        printf("%s\n", "determination of covariance matrix fails");
        exit(0);
    }
    printf("%s\n", "covariance matrix CX = ");
    mtxwrt_(com.cx, &nred, &nred);
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
/* plot */
/* prepare caption */
    sprintf(capt, "x_1#=%6.3lf, x_2#=%6.3lf, x_3#=%6.3lf,"
            " x_4#=%6.3lf, x_5#=%6.3lf", com.x[0], com.x[1],
            com.x[2], com.x[3], com.x[4]);
    lcapt = strlen(capt);
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    strncpy(tx, "y", 1);
    ltx = 1;
    strncpy(ty, "N(y)", 4);
    lty = 4;
/* draw curve corresponding to solution */
    fact = (double) cminbh.nevent * cminbh.deltat;
    dpl = .020520520520520519;
    for (ipl = 1; ipl <= 1000; ++ipl) {
        com.xpl[ipl - 1] = (double) (ipl - 1) * dpl - 10.25;
        com.ypl[ipl - 1] = fact * (com.x[0] * brewig(&com.xpl[ipl - 1],
                 &com.x[1], &com.x[2]) + (1. - com.x[0]) * brewig(&
                com.xpl[ipl - 1], &com.x[3], &com.x[4]));
    }
    (void)getchar();
    grhscv_(com.xpl, com.ypl, &c__1000, cminbh.hist, &c_dm10p25, &cminbh.deltat,
           &cminbh.nt, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
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
double LIBFUNC minblp(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;
    double ret_val, d__1;

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
        gi = x[1] * brewig(&cminbh.t[i - 1], &x[2], &x[3]) + (1. - x[1]) *
                brewig(&cminbh.t[i - 1], &x[4], &x[5]);
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
double LIBFUNC minbsq(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;
    double ret_val, d__1;

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
        gi = x[1] * brewig(&cminbh.t[i - 1], &x[2], &x[3]) + (1. - x[1]) *
                brewig(&cminbh.t[i - 1], &x[4], &x[5]);
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

/* ------------------------------------------------------------------ */
double LIBFUNC brewig(double const P_T *x, double const P_T *a,
                      double const P_T *gamma)
{
    /* System generated locals */
    double ret_val, d__1, d__2;

/* yields Breit-Wigner function at point X with mean A and FWHM GAMMA */
/* Computing 2nd power */
    d__1 = *x - *a;
/* Computing 2nd power */
    d__2 = *gamma;
    ret_val = abs(*gamma) * .6366198 / (d__1 * d__1 * 4. + d__2 * d__2);
    return ret_val;
} /* brewig */

