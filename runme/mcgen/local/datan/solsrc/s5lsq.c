#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern double CBFUNC bwfnct(double const P_T *x, integer const P_T *nr,
                            double const P_T *t);
extern double CBFUNC gsfnct(double const P_T *x, integer const P_T *nr,
                            double const P_T *t);

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__19 = 19, c__21 = 21;
static double c_d0 = 0., c_dp2 = .2, c_dp9 = .9, c_dp17 = .17, c_dp87 = .87,
              c_dmp414 = -.414, c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, iswit, lcapt, list[2], nstep, nws, nx, ny, nr = 2;
    static double cx[4] /* was [2][2] */, dx, dy, deltay[21], dx1, dx2,
                  dxplus[2], dxmins[2], fcont, scrat[4] /* was [2][2] */,
                  p, r, t[21], x[2], y[21], sigma, rho, xpl[2], ypl[2],
                  xmin, ymin, xmax, ymax, a[42] /* was [21][2] */;
    static char capt[75], string[75];

/* identify program to user */
    printf("%s\n", "Program S5LSQ generates data");
    printf("%s\n", "corresponding to a Breit-Wigner law");
    printf("%s\n", "and fits a Breit-Wigner  OR a Gaussian.");
    printf("%s\n", "It shows fitted parameters with symmetric");
    printf("%s\n\n", "and asymmetric errors and confidence region.");
/* ask for input */
    printf("%s\n", "Enter size of measurement errors (> 0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - Fit to Breit-Wigner function");
    printf("%s\n", "2 - Fit to Gaussian");
    printf("%s", "> ");
    scanf("%li", &iswit);
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* generate data points corresponding to Breit-Wigner */
    x[0] = 0.;
    x[1] = 1.;
    rnstnr_(deltay, &c__21);
    for (i = 1; i <= 21; ++i) {
        t[i - 1] = (double) (i - 1) * .3 - 3.;
        y[i - 1] = bwfnct(x, &nr, &t[i - 1]);
        y[i - 1] += deltay[i - 1] * sigma;
        deltay[i - 1] = sigma;
    }
/* set first approximation of unknowns */
    x[0] = .5;
    x[1] = .5;
/* perform fit */
    nstep = 100;
    if (iswit == 1) {
        lsqnon_(bwfnct, t, y, deltay, &c__21, &nr, &nr, list, x,
               cx, &r, a, scrat, &nstep);
    } else {
        lsqnon_(gsfnct, t, y, deltay, &c__21, &nr, &nr, list, x,
               cx, &r, a, scrat, &nstep);
    }
    p = 1. - scchi2_(&r, &c__19);
    if (nstep < 0) {
        printf("%s%5li\n", "LSQNON ended with NSTEP = ", nstep);
    } else {
/* compute asymmetric errors */
        nstep = 100;
        if (iswit == 1) {
            lsqasn_(bwfnct, t, y, deltay, &c__21, &nr, &nr, list,
                   x, cx, &r, &c_d0, dxplus, dxmins, a, scrat, &nstep);
        } else {
            lsqasn_(gsfnct, t, y, deltay, &c__21, &nr, &nr, list,
                   x, cx, &r, &c_d0, dxplus, dxmins, a, scrat, &nstep);
        }
/* prepare graphic */
        sprintf(capt, "x_1#=%6.2lf, x_2#=%6.2lf, M=%6.2lf, P=%8.6lf",
                x[0], x[1], r, p);
        if (iswit == 1) {
            strncpy(capt + 46, " (fit to Breit-Wigner) ", 23);
        } else {
            strncpy(capt + 46, " (fit to Gaussian)     ", 23);
        }
        lcapt = strlen(capt);
        dx1 = sqrt(cx[0]);
        dx2 = sqrt(cx[3]);
        rho = cx[2] / (dx1 * dx2);
        xmin = x[0] - dx1 * 2.;
        ymin = x[1] - dx2 * 2.;
        xmax = x[0] + dx1 * 2.;
        ymax = x[1] + dx2 * 2.;
        gropen_();
        gropws_(&nws);
        grwncc_(&xmin, &xmax, &ymin, &ymax);
        grvwwc_(&c_dp2, &c_dp9, &c_dp17, &c_dp87);
        grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
        grfram_();
        grboun_();
        grtxtc_(&c_d1, capt, &lcapt);
        memset(string, ' ', 75);
        strncpy(string, "x_1", 3);
        grsclx_(string, &c__3);
        strncpy(string, "x_2", 3);
        grscly_(string, &c__3);
        grstcl_(&c__2);
        grdatp_(&c__1, &c_d1, x, &x[1], &dx1, &dx2, &rho);
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
        fcont = r + 1.;
        grstcl_(&c__4);
        nx = 100;
        ny = 100;
        dx = (xmax - xmin) / nx;
        dy = (ymax - ymin) / ny;
        if (iswit == 1) {
            lsqcon_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont, x,
                   t, y, deltay, &c__21, &nr, bwfnct);
        } else {
            lsqcon_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont, x,
                   t, y, deltay, &c__21, &nr, gsfnct);
        }
        grclse_();
    }
    return 0;
} /* main */

/* ------------------------------------------------- */
double CBFUNC bwfnct(double const P_T *x, integer const P_T *nr,
                     double const P_T *t)
{
    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

    /* Parameter adjustments */
    --x;

    /* Function Body */
/* Computing 2nd power */
    d__1 = x[2];
/* Computing 2nd power */
    d__2 = *t - x[1];
/* Computing 2nd power */
    d__3 = x[2];
    ret_val = d__1 * d__1 * 2. / (x[2] * 3.14159f * (d__2 * d__2 * 4. + d__3 *
             d__3));
    return ret_val;
} /* bwfnct */

/* ------------------------------------------------- */
double CBFUNC gsfnct(double const P_T *x, integer const P_T *nr,
                     double const P_T *t)
{
    /* System generated locals */
    double ret_val, d__1;

    /* Parameter adjustments */
    --x;

    /* Function Body */
/* Computing 2nd power */
    d__1 = (*t - x[1]) / x[2];
    ret_val = .39894228 / x[2] * exp(d__1 * d__1 * -.5);
    return ret_val;
} /* gsfnct */

