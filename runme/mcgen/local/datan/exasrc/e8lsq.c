#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Common Block Declarations */

struct grvars {
    double x[2], y[8], cy[64] /* was [8][8] */, yy[8], gy[64] /* was [8][8] */,
           cx[4] /* was [2][2] */, cyy[64] /* was [8][8] */,
           f[100] /* was [10][10] */, e[40] /* was [4][10] */,
           a2[100] /* was [10][10] */;
    integer list[2];
    double dxplus[2], dxmins[2], xpl[2], ypl[2];
} LIBDATA grvars;

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__8 = 8, c__75 = 75;
static double c_dp2 = .2, c_dp9 = .9, c_dmp414 = -.414, c_d1 = 1., c_d0 = 0., 
              c_dp5 = .5;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer i, m, n, nstep, nred, nr, nx, ny, nws;
    static double t[4] = { .2,.2,.8,.85 }, s[4] = { .15,.7,1.1,.8 },
                  dt[4] = { .4,.4,.4,.2 }, ds[4] = { .2,.4,.4,.4 },
                  rho[4] = { 0.,0.,.5,0. }, xmin, ymin, xmax, ymax,
                  r, w, fcont, x1, x2, rh, dx, dy, dx1, dx2;
    static char yes1[1] = "Y", yes2[1] = "y", string[75], answer[1];

    /* System generated locals */
    double d__1;

/* identify program to user */
    printf("%s\n", "Program E8LSQ demonstrates");
    printf("%s\n\n", "use of LSQASG and LSQCOG");
/* write table of data */
    printf("%s\n", "    T         S         DT        DS        RHO");
    for (i = 1; i <= 4; ++i) {
    printf("%10.5lf%10.5lf%10.5lf%10.5lf%10.5lf\n",
           t[i-1], s[i-1], dt[i-1], ds[i-1], rho[i-1]);
    }
    printf("\n");
/* set up data for input to LSQGEN */
    n = 8;
    m = 4;
    nr = 2;
    mtxunt_(grvars.cy, &c__8);
    for (i = 1; i <= 4; ++i) {
	grvars.y[(i - 1) * 2] = t[i - 1];
	grvars.y[(i - 1 << 1) + 1] = s[i - 1];
/* Computing 2nd power */
	d__1 = dt[i - 1];
	grvars.cy[(i - 1 << 1) + 1 + ((i - 1 << 1) + 1 << 3) - 9] = d__1 * d__1;
/* Computing 2nd power */
	d__1 = ds[i - 1];
	grvars.cy[(i - 1 << 1) + 2 + ((i - 1 << 1) + 2 << 3) - 9] = d__1 * d__1;
	grvars.cy[(i - 1 << 1) + 1 + ((i - 1 << 1) + 2 << 3) - 9] = rho[i - 1]
                 * ds[i - 1] * dt[i - 1];
	grvars.cy[(i - 1 << 1) + 2 + ((i - 1 << 1) + 1 << 3) - 9] = rho[i - 1]
                 * ds[i - 1] * dt[i - 1];
    }
/* save input data */
    mtxcpv_(grvars.y, grvars.yy, &n);
    mtxtra_(grvars.cy, grvars.cyy, &n, &n);
/* determine first approximation */
    nred = 2;
    grvars.list[0] = 1;
    grvars.list[1] = 1;
    grvars.x[1] = (s[3] - s[0]) / (t[3] - t[0]);
    grvars.x[0] = s[0] - grvars.x[1] * t[0];
/* perform fit with LSQGEN */
    printf("%s\n", "performing fit with LSQGEN");
    printf("%s%2li%s%2li%s%2li%s%2li%2li\n", "N = ", n, ", NR = ", nr,
           ", NRED = ", nred, ", LIST = ", grvars.list[0], grvars.list[1]);
    printf("%s%10.5lf%10.5lf\n", "first approx.: X = ",
           grvars.x[0], grvars.x[1]);
    nstep = 100;
    lsqgen_(grvars.y, grvars.cy, grvars.gy, grvars.f, grvars.e, &m, 
	    &n, &nr, &nred, grvars.list, grvars.x, grvars.cx, &r, 
	    grvars.a2, &nstep);
    printf("%s%6.2lf%s%3li\n%s%10.5lf%10.5lf\n", "result of fit: R = ", r,
           ", NSTEP =", nstep, "X =", grvars.x[0], grvars.x[1]);
    printf("%s\n", "covariance matrix CX = ");
    mtxwrt_(grvars.cx, &nred, &nred);
    printf("\n");
/* store solution, errors and correlation for later drawing */
    x1 = grvars.x[0];
    x2 = grvars.x[1];
    dx1 = sqrt(grvars.cx[0]);
    dx2 = sqrt(grvars.cx[3]);
    rh = grvars.cx[2] / (dx1 * dx2);
/* restore input data and determine asymmetric errors */
    mtxcpv_(grvars.yy, grvars.y, &n);
    mtxtra_(grvars.cyy, grvars.cy, &n, &n);
    nstep = 100;
    w = 0.;
    lsqasg_(grvars.y, grvars.cy, grvars.gy, grvars.f, grvars.e, &m, 
	    &n, &nr, &nred, grvars.list, grvars.x, grvars.cx, &r, &w, 
	    grvars.dxplus, grvars.dxmins, grvars.a2, &nstep);
    printf("%s\n", "asymmetric errors:");
    printf("%s", "DXPLUS = ");
    for (i = 1; i <= nred; ++i)
        printf("%10.5lf", grvars.dxplus[i-1]);
    printf("\n");
    printf("%s", "DXMINS = ");
    for (i = 1; i <= nred; ++i)
        printf("%10.5lf", grvars.dxmins[i-1]);
    printf("\n");
    printf("%s\n", "press Y to see plot of confidence region");
    printf("%s\n", "press N to stop  ");
    scanf("%1s", answer);
    if (*answer == yes1[0] || *answer == yes2[0]) {
/* plot confidence region */
/* ask for number of workstation */
        grnbws_();
        printf("%s\n", "Please, enter number of workstation:");
        printf("%s", "> ");
        scanf("%ld", &nws);
        (void)getchar();
	gropen_();
	gropws_(&nws);
	xmin = -1.5;
	ymin = 0.;
	xmax = 1.5;
	ymax = 3.;
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
/* draw solution with symmetric errors and covariance ellipse */
	grstcl_(&c__2);
	grdatp_(&c__1, &c_dp5, &x1, &x2, &dx1, &dx2, &rh);
/* draw asymmetric errors */
	grstcl_(&c__3);
	for (i = 1; i <= 2; ++i) {
	    if (i == 1) {
		grvars.xpl[0] = grvars.x[0] - grvars.dxmins[0];
	    } else {
		grvars.xpl[0] = grvars.x[0] + grvars.dxplus[0];
	    }
	    grvars.xpl[1] = grvars.xpl[0];
	    grvars.ypl[0] = ymin;
	    grvars.ypl[1] = ymax;
	    grplin_(&c__2, grvars.xpl, grvars.ypl);
	}
	for (i = 1; i <= 2; ++i) {
	    if (i == 1) {
		grvars.ypl[0] = grvars.x[1] - grvars.dxmins[1];
	    } else {
		grvars.ypl[0] = grvars.x[1] + grvars.dxplus[1];
	    }
	    grvars.ypl[1] = grvars.ypl[0];
	    grvars.xpl[0] = xmin;
	    grvars.xpl[1] = xmax;
	    grplin_(&c__2, grvars.xpl, grvars.ypl);
	}
	fcont = r + 1.;
	nx = 30;
	ny = 30;
	dx = (xmax - xmin) / nx;
	dy = (ymax - ymin) / ny;
	grstcl_(&c__4);
/* retore input data */
	mtxcpv_(grvars.yy, grvars.y, &n);
	mtxtra_(grvars.cyy, grvars.cy, &n, &n);
/* draw confidence region */
	lsqcog_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont, 
	       grvars.y, grvars.cy, grvars.gy, grvars.f, grvars.e, 
	       &m, &n, &nr, grvars.x, grvars.a2);
	grclse_();
    }
    return 0;
} /* main */

