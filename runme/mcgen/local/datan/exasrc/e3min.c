#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Common Block Declarations */

struct cmings_ {
    double y[1000];
    integer nny;
} LIBDATA cmings_;


/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__38 = 38;
static double c_dp2 = .2, c_dp9 = .9, c_dp15 = .15, c_dp85 = .85, 
              c_dmp414 = -.414, c_d1 = 1., c_d0 = 0., c_dp5 = .5;

main(void)
{
    /* Local variables */
    static integer i, list[2], nstep, nseed1, nseed2, nx, ny, nws;
    static double fact, fmin, xmin, xmax, ymin, ymax, x[2], fcont,
                  scrat[6] /* was [2][3] */, scrat1[4] /* was [2][2] */,
                  cx[4] /* was [2][2] */, rh, dx, dy, epsiln, dxmins[2],
                  dx1, dx2, dxplus[2], xpl[2], ypl[2];
    static char string[75];
    static logical ok;

/* identify program to user */
    printf("%s\n", "Program E3MIN demonstrates use of");
    printf("%s\n\n", "MINASY and MINCNT.");
    printf("%s\n", "Enter number of events (>1):");
    printf("%s", "> ");
    scanf("%li", &cmings_.nny);
/* draw sample */
    nseed1 = 1;
    nseed2 = 2;
    rne2in_(&nseed1, &nseed2);
    rnstnr_(cmings_.y, &cmings_.nny);
/* determine first approximation */
    list[0] = 1;
    list[1] = 1;
    x[0] = 1.;
    x[1] = 2.;
/* minimize with MINSIM */
    nstep = 10000;
    epsiln = 0.;
    fmin = .01;
    printf("%s\n", "minimization with MINSIM");
    printf("%s%2li%s%2li%s%2li%s%2li%2li\n", "N = ", cmings_.nny,
           ", NR = ", c__2, ", NRED = ", c__2, ", LIST = ", list[0], list[1]); 
    printf("%s%10.5lf%10.5lf\n", "first approx.: X = ", x[0], x[1]); 
    minsim_(x, &c__2, &c__2, list, mingls_, &fmin, &epsiln, &nstep, scrat);
    printf("%s%6.2lf%s%6li\n%s%10.5lf%10.5lf\n",
          "result of minimization: FMIN = ", fmin, ", NSTEP =", nstep,
          "X =", x[0], x[1]);
/* determine covariance matrix */
    fact = 1.;
    mincov_(x, &c__2, &c__2, list, mingls_, &fact, scrat, scrat1, cx, &ok);
    if (! ok) {
        printf("%s\n", "determination of covariance matrix fails");
	exit(0);
    }
    printf("%s\n", "covariance matrix CX = ");
    mtxwrt_(cx, &c__2, &c__2);
/* prepare size of plot */
    xmin = x[0] - sqrt(cx[0]) * 2.;
    xmax = x[0] * 2. - xmin;
    ymin = x[1] - sqrt(cx[3]) * 2.;
    ymax = x[1] * 2. - ymin;
/* determine asymmetric errors */
    fcont = fmin + .5;
    minasy_(mingls_, &c__2, &c__2, list, x, cx, &fcont, dxplus, dxmins, 
	   scrat, &nstep);
    printf("\n%s\n", "asymmetric errors:");
    printf("%s%10.5lf%10.5lf\n", "DXPLUS = ", dxplus[0], dxplus[1]);
    printf("%s%10.5lf%10.5lf\n", "DXMINS = ", dxmins[0], dxmins[1]);
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* plot confidence region */
    gropen_();
    gropws_(&nws);
    grwncc_(&xmin, &xmax, &ymin, &ymax);
    grvwwc_(&c_dp2, &c_dp9, &c_dp15, &c_dp85);
    grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
    grfram_();
    grboun_();
    memset(string, ' ', 75);
    sprintf(string, "N = %4li, x_1# = %6.3lf, x_2# = %6.3lf", cmings_.nny,
            x[0], x[1]);
    string[38] = ' ';
    grtxtc_(&c_d1, string, &c__38);
    strncpy(string, "x_1", 3);
    grsclx_(string, &c__3);
    strncpy(string, "x_2", 3);
    grscly_(string, &c__3);
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
    mincnt_(&c__1, &c__2, &xmin, &ymin, &dx, &dy, &nx, &ny, &fcont, x, &c__2, 
	   mingls_);
    grclse_();
    return 0;
} /* main */

