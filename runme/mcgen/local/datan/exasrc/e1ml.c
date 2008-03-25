#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern double CBFUNC fue1ml(double const P_T *tau, double const P_T *tbar,
                            integer const P_T *n, integer const P_T *ndum);

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__4 = 4, c__11 = 11, c__71 = 71,
               c__500 = 500;
static double c_d0 = 0., c_d5 = 5., c_dp6 = .6, c_dp1 = .1, c_d1p1 = 1.1, 
              c_dp75 = .75, c_dmp214 = -.214, c_d1p2 = 1.2, c_dmp1 = -.1, 
              c_dp9 = .9, c_d1em5 = 1e-5, c_d1 = 1.;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer ndum, i, n, iseed1, iseed2, nws;
    static double tbar, fmin, t[1000], x0, x1, sigmns, sigpls, sig,
                  xpl[500], ypl[500];
    static char capt[75], tx[75] =
                "t,&t                                              "
	        "                         ",
                ty[75] = "-(l-l_max#)                                       "
	        "                         ";
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program E1ML computes mean life and");
    printf("%s\n\n", "asymmetric errors from few radioactive decays.");
/* ask for input */
    printf("%s\n", "Enter number N of decays (1 <= N <= 1000):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter seed number ISEED1 (>0):");
    printf("%s", "> ");
    scanf("%li", &iseed1);
    printf("%s\n", "Enter seed number ISEED2 (>0):");
    printf("%s", "> ");
    scanf("%li", &iseed2);
/* initialization */
    if (iseed1 <= 0) {
	iseed1 = 123456;
    }
    if (iseed2 <= 0) {
	iseed2 = 654321;
    }
    rne2in_(&iseed1, &iseed2);
/* prepare graphics */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
    gropen_();
    gropws_(&nws);
    grwncc_(&c_d0, &c_d5, &c_d0, &c_dp6);
    grvwwc_(&c_dp1, &c_d1p1, &c_dp1, &c_dp75);
    grwnwc_(&c_dmp214, &c_d1p2, &c_dmp1, &c_dp9);
    grstfr_(&nws, &c_d5, &c_d0);
    grfram_();
    grsclx_(tx, &c__4);
    grscly_(ty, &c__11);
    grboun_();
/* generate sample and draw 1D scatter diagram */
    rnecuy_(t, &n);
    tbar = 0.;
    ypl[0] = 0.;
    ypl[1] = .1;
    grstcl_(&c__2);
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	t[i - 1] = -log(t[i - 1]);
	tbar += t[i - 1];
	xpl[0] = t[i - 1];
	xpl[1] = t[i - 1];
	grplin_(&c__2, xpl, ypl);
    }
/* draw line -(l-lmax)=.5 */
    tbar /= (double) n;
    xpl[0] = 0.;
    xpl[1] = 5.;
    ypl[0] = .5;
    ypl[1] = .5;
    grplin_(&c__2, xpl, ypl);
/* draw short line at tau=tbar */
    xpl[0] = tbar;
    xpl[1] = tbar;
    ypl[0] = .52;
    ypl[1] = .48;
    grstcl_(&c__4);
    grplin_(&c__2, xpl, ypl);
/* draw likelihood function -(l-lmax) */
    fmin = (double) n * (log(tbar) + 1.f);
    for (i = 1; i <= 500; ++i) {
	xpl[i - 1] = (double) i * .01;
	ypl[i - 1] = (double) n * (tbar / xpl[i - 1] + log(xpl[i - 1])) - fmin;
    }
    grplin_(&c__500, xpl, ypl);
/* compute asymmetric errors */
    x0 = .001;
    x1 = tbar;
    auxzbr_(&x0, &x1, fue1ml, &tbar, &n, &ndum);
    auxzfn_(&x0, &x1, &sigmns, fue1ml, &tbar, &n, &ndum, &c_d1em5);
    sigmns = tbar - sigmns;
    x0 = tbar;
    x1 = 5.;
    auxzbr_(&x0, &x1, fue1ml, &tbar, &n, &ndum);
    auxzfn_(&x0, &x1, &sigpls, fue1ml, &tbar, &n, &ndum, &c_d1em5);
    sigpls -= tbar;
/* compute symmetric error */
    sig = tbar / sqrt((double) n);
/* prepare and draw caption */
    sprintf(capt, "N =%4li, t^^\"-# = %4.2lf, &D@_-# = %5.2lf,"
            " &D@_+# = %5.2lf, &Dt^\"~# = %5.2lf",
            n, tbar, sigmns, sigpls, sig);
    memset(capt + 71, ' ', 4);
    grstcl_(&c__1);
    grtxtc_(&c_d1, capt, &c__71);
    grclse_();
    return 0;
} /* main */

/* ----------------------------------------------------------- */
double CBFUNC fue1ml(double const P_T *tau, double const P_T *tbar,
                     integer const P_T *n, integer const P_T *ndum)
{
    /* System generated locals */
    double ret_val;

    ret_val = (double) (*n) * (*tbar / *tau + log(*tau / *tbar) - 1.) - .5;
    return ret_val;
} /* fue1ml */

