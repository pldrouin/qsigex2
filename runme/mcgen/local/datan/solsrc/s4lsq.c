#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

extern double CBFUNC gsfnct(double const P_T *x, integer const P_T *nr,
                            double const P_T *t);
extern double CBFUNC bwfnct(double const P_T *x, integer const P_T *nr,
                            double const P_T *t);

/* Table of constant values */

static integer c__1 = 1, c__19 = 19, c__21 = 21;
static double c_dp5 = .5;

main(void)
{
    /* Local variables */
    static integer i, ipl, npl, lcapt, list[2], ltx, lty, nws, nstep, iswit,
                  nr = 2;
    static double a[42] /* was [21][2] */, cx[4] /* was [2][2] */,
                  datcov[21], deltay[21], p, r, t[21], x[2], y[21], sigma,
                  scrat[4] /* was [2][2] */, datsx[21], xpl[100], ypl[100];
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n", "Program S4LSQ generates data");
    printf("%s\n", "corresponding to a Breit-Wigner law");
    printf("%s\n\n", "and fits a Breit-Wigner OR a Gaussian.");
/* ask for input */
    printf("%s\n", "Enter size of measurement errors (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - Fit to Breit-Wigner function");
    printf("%s\n", "2 - Fit to Gaussian");
    printf("%s", "> ");
    scanf("%li", &iswit);
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
/* prepare graphics */
        memset(tx, ' ', 75);
        memset(ty, ' ', 75);
	strncpy(tx, "t", 1);
	strncpy(ty, "y", 1);
	ltx = 1;
	lty = 1;
	sprintf(capt, "x_1#=%6.2lf, x_2#=%6.2lf, M=%6.2lf, P=%8.6lf",
                x[0], x[1], r, p);
	if (iswit == 1) {
            strncpy(capt + 46, " (fit to Breit-Wigner) ", 23);
	} else {
            strncpy(capt + 46, " (fit to Gaussian)     ", 23);
	}
        memset(capt + 69, ' ', 6);
	lcapt = 69;
/* curve corresponding to solution */
	npl = 100;
	i__1 = npl;
	for (ipl = 1; ipl <= i__1; ++ipl) {
	    xpl[ipl - 1] = (double) ipl * .08 - 4.;
	    if (iswit == 1) {
		ypl[ipl - 1] = bwfnct(x, &nr, &xpl[ipl - 1]);
	    } else {
		ypl[ipl - 1] = gsfnct(x, &nr, &xpl[ipl - 1]);
	    }
	}
/* prepare data points for graphical presentation */
	for (i = 1; i <= 21; ++i) {
	    datsx[i - 1] = 0.;
	    datcov[i - 1] = 0.;
	}
/* ask for number of workstation */
        grnbws_();
        printf("%s\n", "Please, enter number of workstation:");
        printf("%s", "> ");
        scanf("%li", &nws);
        (void)getchar();
/* graphical output */
	grdtcv_(xpl, ypl, &npl, &c__1, &c_dp5, t, y, datsx, deltay, datcov,
               &c__21, tx, &ltx, ty, &lty, capt, &lcapt, &nws);
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

