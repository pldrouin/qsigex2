#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern double CBFUNC minbwl(double const P_T *x, integer const P_T *n);
extern void LIBFUNC rnbw(double const P_T *a, double const P_T *gamma,
                         double P_T *r, integer const P_T *n);

/* Common Block Declarations */

struct cminbw {
    double y[1000];
    integer nny;
} LIBDATA cminbw;

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__74 = 74,
               c__1000 = 1000;
static double c_dp15 = .15, c_dp85 = .85, c_dmp414 = -.414, c_d1 = 1.,
              c_d0 = 0., c_dmp2 = -.2, c_dp9 = .9;

main(void)
{
    /* Local variables */
    static integer i, k, ipl, list[2], nseed1, nseed2, nstep, nws;
    static double cx[4] /* was [2][2] */, dpl, epsiln, fact, dels, fmin,
                  gamma, x[2], delxm, scrat[6] /* was [2][3] */, s2, a,
                  scrat1[4] /* was [2][2] */, xmin, xmax, ymin, ymax, dels2,
                  xpl[1000], ypl[1000];
    static logical ok;
    static char string[75];
    /* System generated locals */
    integer i__1;
    double d__1, d__2, d__3;
/* identify program to user */
    printf("%s\n", "Program S2MIN performs fit of Breit-Wigner");
    printf("%s\n\n", "to small sample by minimization.");
/* ask for input */
    printf("%s\n", "Enter number n of events (2 <= n <= 1000):");
    printf("%s", "> ");
    scanf("%li", &cminbw.nny);
/* draw sample from Breit-Wigner distribution */
    nseed1 = 87654;
    nseed2 = 98765;
    rne2in_(&nseed1, &nseed2);
    a = 0.;
    gamma = 1.;
    rnbw(&a, &gamma, cminbw.y, &cminbw.nny);
/* write table of data */
    printf("%s\n", "sample Y is");
    for (k = 1; k <= cminbw.nny; ++k) {
        if (k % 10 == 1)
            printf(" ");
        printf("%7.2lf", cminbw.y[k-1]);
        if (k % 10 == 0)
            printf("\n");
    }
    if (cminbw.nny % 10 != 0)
        printf("\n");
/* find first approximation */
    smmnvr_(cminbw.y, &cminbw.nny, x, &delxm, &s2, &dels2, &x[1], &dels);
/* minimize with MINSIM */
    list[0] = 1;
    list[1] = 1;
    nstep = 10000;
    epsiln = 0.;
    fmin = .01;
    printf("%s\n", "minimization with MINSIM");
    printf("%s%4li%s%4li%s%4li%s%2li%2li\n", "N = ", cminbw.nny,
           ", NR = ", c__2, ", NRED = ", c__2, ", LIST = ", list[0], list[1]);
    printf("%s%10.5lf%10.5lf\n", "first approx.: X = ", x[0], x[1]);
    minsim_(x, &c__2, &c__2, list, minbwl, &fmin, &epsiln, &nstep, scrat);
    printf("%s%6.2lf%s%6li\n%s%10.5lf%10.5lf\n",
           "result of minimization: FMIN = ", fmin, ", NSTEP =", nstep,
           "X =", x[0], x[1]);
/* determine covariance matrix */
    fact = 1.;
    mincov_(x, &c__2, &c__2, list, minbwl, &fact, scrat, scrat1, cx, &ok);
    if (! ok) {
        printf("%s\n", "determination of covariance matrix fails");
        exit(0);
    }
    printf("%s\n", "covariance matrix CX = ");
    mtxwrt_(cx, &c__2, &c__2);
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* prepare graphics */
    xmin = -10.;
    xmax = 10.;
    ymin = 0.;
    ymax = 1.;
    gropen_();
    gropws_(&nws);
    grwncc_(&xmin, &xmax, &ymin, &ymax);
    grvwwc_(&c_dmp2, &c_dp9, &c_dp15, &c_dp85);
    grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
    grfram_();
    grboun_();
    sprintf(string, "N = %4li, x_1# = %6.3lf, x_2# = %6.3lf"
            ", &D@x_1# = %6.3lf, &D@x_2# = %6.3lf", cminbw.nny, x[0], x[1],
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
    i__1 = cminbw.nny;
    for (i = 1; i <= i__1; ++i) {
        xpl[0] = cminbw.y[i - 1];
        xpl[1] = xpl[0];
        ypl[0] = 0.;
        ypl[1] = .1;
        grplin_(&c__2, xpl, ypl);
    }
/* draw Breit-Wigner corresponding to solution */
    fact = 2. / (x[1] * 3.14159);
    dpl = (xmax - xmin) / 999.;
    for (ipl = 1; ipl <= 1000; ++ipl) {
        xpl[ipl - 1] = xmin + (double) (ipl - 1) * dpl;
/* Computing 2nd power */
        d__1 = x[1];
/* Computing 2nd power */
        d__2 = xpl[ipl - 1] - x[0];
/* Computing 2nd power */
        d__3 = x[1];
        ypl[ipl - 1] = fact * (d__1 * d__1) / (d__2 * d__2 * 4. + d__3 * d__3);
    }
    grstcl_(&c__4);
    grplin_(&c__1000, xpl, ypl);
    grclse_();
    return 0;
} /* main */

/* -------------------------------------------- */
double CBFUNC minbwl(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;
    double ret_val, d__1, d__2;

    /* Local variables */
    static double f;
    static integer i;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    f = (double) cminbw.nny * (log(2.) - log(3.14159) + log((abs(x[2]))))
            ;
    i__1 = cminbw.nny;
    for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
        d__1 = cminbw.y[i - 1] - x[1];
/* Computing 2nd power */
        d__2 = x[2];
        f -= log(d__1 * d__1 * 4. + d__2 * d__2);
    }
    ret_val = -f;
    return ret_val;
} /* minbwl */

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

