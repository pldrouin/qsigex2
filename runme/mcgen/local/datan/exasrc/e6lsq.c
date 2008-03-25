#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__4 = 4, c__2 = 2, c__20 = 20, c__75 = 75;
static double c_d0 = 0., c_dp2 = .2, c_dp9 = .9, c_dmp414 = -.414, c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, nseed1, nseed2, nstep, nred, list[2], nx, ny, nws;
    static double xmin, ymin, xmax, ymax, a[40] /* was [20][2] */,
                  cx[4] /* was [2][2] */, dx, dy, gx[4] /* was [2][2] */,
                  r, t[20], x[2], y[20], sigma, fcont, deltay[20], dxmins[2],
                  dx1, dx2, dxplus[2], sig, rho, xpl[2], ypl[2], delt;
    static char string[75];

/* identify program to user */
    printf("%s\n\n", "Program E6LSQ demonstrates use of LSQASN.");
/* create simulated data */
    x[0] = 10.;
    x[1] = 1.;
    nseed1 = 15;
    nseed2 = 211;
    rne2in_(&nseed1, &nseed2);
    rnstnr_(deltay, &c__20);
    rnecuy_(y, &c__20);
    delt = .09523809523809523;
    sigma = 2.;
    for (i = 1; i <= 20; ++i) {
	t[i - 1] = (double) i * delt;
	sig = sigma * (.5 + y[i - 1]);
	y[i - 1] = lsqexp_(x, &c__2, &t[i - 1]) + deltay[i - 1] * sig;
	deltay[i - 1] = sig;
    }
/* set first approximation of unknowns and perform fit with LSQNON */
    nred = 2;
    x[0] = .1;
    x[1] = .1;
    list[0] = 1;
    list[1] = 1;
    nstep = 100;
    lsqnon_(lsqexp_, t, y, deltay, &c__20, &c__2, &nred, list, x, cx, &r, a,
           gx, &nstep);
    nstep = 100;
/* compute asymmetric errors */
    lsqasn_(lsqexp_, t, y, deltay, &c__20, &c__2, &nred, list, x, cx, &r,
           &c_d0, dxplus, dxmins, a, gx, &nstep);
/* prepare graphics */
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%ld", &nws);
    (void)getchar();
    gropen_();
    gropws_(&nws);
    xmin = 5.;
    ymin = .5;
    xmax = 15.;
    ymax = 1.5;
    grwncc_(&xmin, &xmax, &ymin, &ymax);
    grvwwc_(&c_dp2, &c_dp9, &c_dp2, &c_dp9);
    grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
    grfram_();
    grboun_();
    memset(string, ' ', 75);
    strncpy(string, "x_1", 3);
    grsclx_(string, &c__75);
    strncpy(string, "x_2", 3);
    grscly_(string, &c__75);
    dx1 = sqrt(cx[0]);
    dx2 = sqrt(cx[3]);
    rho = cx[2] / (dx1 * dx2);
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
/* compute and draw contour with LSQCON */
    lsqcon_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont, x, t, y, 
	   deltay, &c__20, &c__2, lsqexp_);
    grclse_();
    return 0;
} /* main */

