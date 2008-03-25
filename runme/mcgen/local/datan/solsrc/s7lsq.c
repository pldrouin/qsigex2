#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern double CBFUNC lsqgfn_(double const P_T *eta, double const P_T *x,
                            integer const P_T *n, integer const P_T *nr,
                            integer const P_T *k);

/* Common Block Declarations */
/* COMMON needed for Microsoft FORTRAN compiler */
struct {
       double x[3], y[40], *cy /* was [40][40] */,
              *gy /* was [40][40] */, cx[9] /* was [3][3] */,
              *e /* was [20][43] */, *f /* was [43][43] */,
              *a2 /* was [43][43] */, t[20], s[20], dt[20], ds[20],
              rho[20], a[2], c[4] /* was [2][2] */,
              dplus[4] /* was [2][2] */, random[2];
       integer list[3];
} com;

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__4 = 4;
static double c_dp2 = .2, c_dp9 = .9, c_dp17 = .17, c_dp87 = .87,
              c_dmp414 = -.414, c_d1 = 1., c_d0 = 0., c_dp25 = .25, c_dp5 = .5,
              c_dm1p5 = -1.5, c_d1p5 = 1.5;

main(void)
{
    /* Local variables */
    static integer ipl, npl, lcapt, nstep, nr, nred, i, m, n, nws, nvar;
    static double delphi, p, r, phi, r0, s0, t0, sa, ta, tb, sb,
                  sigmas, sigmat, slopea, correl, slopeb, xpl[100], ypl[100];
    static char capt[75], string[75];
    /* System generated locals */
    integer i__1;
    double d__1, d__2;

    if ( (com.cy = (double *)MEMALLOC(sizeof(double)*2*1600)) == NULL ||
         (com.e = (double *)MEMALLOC(sizeof(double)*(2*1849+860))) == NULL) {
      MEMERROR("s7lsq");
      return(0);
    } else {
      com.gy = com.cy+1600;
      com.f = com.e+860;
      com.a2 = com.f+1849;
    }
/* identify program to user */
    printf("%s\n", "Program S7LSQ generates data");
    printf("%s\n", "on unit circle with (correlated) errors");
    printf("%s\n\n", "on both coordinates and performs fit.");
/* ask for input */
    printf("%s\n", "Enter number n of points (3 <= n <=20)to be generated:");
    printf("%s", "> ");
    scanf("%li", &m);
    printf("%s\n", "Enter measurement error in abscissa (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigmat);
    printf("%s\n", "Enter measurement error in ordinate (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigmas);
    printf("%s\n", "Enter measurement correlation coefficient (>-1., <1.):");
    printf("%s", "> ");
    scanf("%lf", &correl);
/* simulate data points */
/* Computing 2nd power */
    d__1 = sigmat;
    com.c[0] = d__1 * d__1;
/* Computing 2nd power */
    d__1 = sigmas;
    com.c[3] = d__1 * d__1;
    com.c[2] = correl * sigmas * sigmat;
    com.c[1] = com.c[2];
    nvar = 2;
    rnmnpr_(com.c, com.dplus, &nvar);
    delphi = 6.283185 / (double) m;
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
        phi = (double) (i - 1) * delphi;
        rnmngn_(com.dplus, com.a, com.random, &c__2);
        com.t[i - 1] = cos(phi) + com.random[0];
        com.s[i - 1] = sin(phi) + com.random[1];
        com.dt[i - 1] = sigmat;
        com.ds[i - 1] = sigmas;
        com.rho[i - 1] = correl;
    }
/* set up data for input to LSQGEN */
    n = m << 1;
    nr = 3;
    nred = 3;
    mtxunt_(com.cy, &n);
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
        com.y[(i - 1) * 2] = com.t[i - 1];
        com.y[(i - 1 << 1) + 1] = com.s[i - 1];
/* Computing 2nd power */
        d__1 = com.dt[i - 1];
        com.cy[(i - 1 << 1) + 1 + ((i - 1 << 1) + 1) * 40 - 41] = d__1 *
                d__1;
/* Computing 2nd power */
        d__1 = com.ds[i - 1];
        com.cy[(i - 1 << 1) + 2 + ((i - 1 << 1) + 2) * 40 - 41] = d__1 *
                d__1;
        com.cy[(i - 1 << 1) + 1 + ((i - 1 << 1) + 2) * 40 - 41] = com.rho[
                i - 1] * com.ds[i - 1] * com.dt[i - 1];
        com.cy[(i - 1 << 1) + 2 + ((i - 1 << 1) + 1) * 40 - 41] = com.rho[
                i - 1] * com.ds[i - 1] * com.dt[i - 1];
    }
/* determine first approximation */
    ta = (com.y[0] + com.y[2]) * .5;
    sa = (com.y[1] + com.y[3]) * .5;
    tb = (com.y[2] + com.y[4]) * .5;
    sb = (com.y[3] + com.y[5]) * .5;
    slopea = -(com.y[3] - com.y[1]) / (com.y[2] - com.y[0]);
    slopeb = -(com.y[5] - com.y[3]) / (com.y[4] - com.y[2]);
    s0 = (ta - tb - slopea * sa + slopeb * sb) / (slopeb - slopea);
    t0 = ta + slopea * (s0 - sa);
/* Computing 2nd power */
    d__1 = t0 - com.y[0];
/* Computing 2nd power */
    d__2 = s0 - com.y[1];
    r0 = sqrt(d__1 * d__1 + d__2 * d__2);
    com.x[0] = t0;
    com.x[1] = s0;
    com.x[2] = r0;
    nstep = 100;
    lsqgen_(com.y, com.cy, com.gy, com.f, com.e, &m, &n, &nr, &nred,
           com.list, com.x, com.cx, &r, com.a2, &nstep);
    i__1 = m - nred;
    p = 1. - scchi2_(&r, &i__1);
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* prepare graphics */
    sprintf(capt, "x_1#=%6.2lf, x_2#=%6.2lf, x_3#=%6.2lf, M=%6.2lf, P=%6.4lf",
            com.x[0], com.x[1], com.x[2], r, p);
    lcapt = strlen(capt);
/* graphical output */
    gropen_();
    gropws_(&nws);
    grwncc_(&c_dm1p5, &c_d1p5, &c_dm1p5, &c_d1p5);
    grvwwc_(&c_dp2, &c_dp9, &c_dp17, &c_dp87);
    grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
    grfram_();
    grboun_();
    grtxtc_(&c_d1, capt, &lcapt);
    memset(string, ' ', 75);
    strncpy(string, "t", 1);
    grsclx_(string, &c__1);
    strncpy(string, "s", 1);
    grscly_(string, &c__1);
    grstcl_(&c__2);
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
        grdatp_(&c__1, &c_dp25, &com.t[i - 1], &com.s[i - 1], &com.dt[i - 1],
               &com.ds[i - 1], &com.rho[i - 1]);
    }
    npl = 100;
    grstcl_(&c__4);
/* circle corresponding to first approximation */
    delphi = .063466515151515149;
    i__1 = npl;
    for (ipl = 1; ipl <= i__1; ++ipl) {
        phi = (double) (ipl - 1) * delphi;
        xpl[ipl - 1] = t0 + r0 * cos(phi);
        ypl[ipl - 1] = s0 + r0 * sin(phi);
    }
    grbrpl_(&c__1, &c_dp5, &npl, xpl, ypl);
/* circle corresponding to solution */
    i__1 = npl;
    for (ipl = 1; ipl <= i__1; ++ipl) {
        phi = (double) (ipl - 1) * delphi;
        xpl[ipl - 1] = com.x[0] + com.x[2] * cos(phi);
        ypl[ipl - 1] = com.x[1] + com.x[2] * sin(phi);
    }
    grplin_(&npl, xpl, ypl);
    grclse_();
    return 0;
} /* main */

/* ---------------------------------------------- */
double CBFUNC lsqgfn_(double const P_T *eta, double const P_T *x,
                     integer const P_T *n, integer const P_T *nr,
                     integer const P_T *k)
{
    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

/* fit to circle */
    /* Parameter adjustments */
    --x;
    --eta;

    /* Function Body */
/* Computing 2nd power */
    d__1 = eta[(*k << 1) - 1] - x[1];
/* Computing 2nd power */
    d__2 = eta[*k * 2] - x[2];
/* Computing 2nd power */
    d__3 = x[3];
    ret_val = d__1 * d__1 + d__2 * d__2 - d__3 * d__3;
    return ret_val;
} /* lsqgfn_ */

