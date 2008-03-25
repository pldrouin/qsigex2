#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__15 = 15, c__46 = 46, c__50 = 50;
static double c_dp5 = .5, c_dm1 = -1., c_d1 = 1.;

main(void)
{
    /* Initialized data */
    static double xa[2] = { 0.,0. }, xb[2] = { 10.,10. }, del = .1;
    static char capt[15] = "t = ***** dt   ";
    /* Local variables */
    static integer i, j, k, l, m, ihist, iswit, mt, nt, nws, nx;
    static double delphi, phi, his, rcs[46], dsq, rsn[46], xpl[46], ypl[46],
                  rand[2], epar[2], delx, dinv, hist[100],
                  xlst[100] /* was [2][50] */, xtry[2], d, uipar, h1[2], h2[2],
                  uiperp, v[100] /* was [2][50] */, w[2],
                  x[100] /* was [2][50] */, eperp[2], x0, dd, dx[2],
                  ui[2], vi[2], vj[2], uj[2];
    static char tx[15], ty[15];
    /* System generated locals */
    integer i__1, i__2;
    double d__1, d__2;
/* identify program to user */
    printf("%s\n", "Program S4RN performs");
    printf("%s\n", "Monte Carlo simulation of molecular motion");
    printf("%s\n\n", "and produces graphical output.");
/* initialization */
    d = 1.;
    dsq = d * d;
    delphi = .13962622222222221;
    phi = 0.;
    for (i = 0; i <= 45; ++i) {
	rcs[i] = cos(phi) * .5;
	rsn[i] = sin(phi) * .5;
	phi += delphi;
    }
/* let user choose */
    printf("%s\n", "Enter number of time steps (several 100 or 1000):");
    printf("%s", "> ");
    scanf("%li", &nt);
    mt = nt / 10;
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - display of trajectories");
    printf("%s\n", "2 - display of histograms");
    printf("%s", "> ");
    scanf("%li", &iswit);
    (void)getchar();
    if (iswit == 1) {
/* initialize graphics for trajectories */
	gropen_();
	gropws_(&nws);
	grwncc_(xa, xb, &xa[1], &xb[1]);
	grfram_();
    } else if (iswit == 2) {
/* let user choose type of histogram */
        printf("%s\n", "Choose:");
        printf("%s\n", "1 - histogram of v_1");
        printf("%s\n", "2 - histogram of v_2");
        printf("%s\n", "3 - histogram of |v|");
        printf("%s\n", "4 - histogram of |v|^2 ");
        printf("%s", "> ");
        scanf("%li", &ihist);
        (void)getchar();
/* initialize histograms */
	nx = 100;
        memset(tx, ' ', 15);
        memset(ty, ' ', 15);
	if (ihist == 1) {
	    x0 = -1.;
	    delx = .02;
	    strncpy(tx, "v_1", 3);
	    strncpy(ty, "N(v_1#)", 7);
	} else if (ihist == 2) {
	    x0 = -1.;
	    delx = .02;
	    strncpy(tx, "v_2", 3);
	    strncpy(ty, "N(v_2#)", 7);
	} else if (ihist == 3) {
	    x0 = 0.;
	    delx = .01;
	    strncpy(tx, "v", 1);
	    strncpy(ty, "N(v)", 4);
	} else if (ihist == 4) {
	    x0 = 0.;
	    delx = .01;
	    strncpy(tx, "v^2", 3);
	    strncpy(ty, "N(v^2#)", 7);
	}
    }
/* choose initial positions */
    for (i = 1; i <= 50; ++i) {
L10:
	rnecuy_(rand, &c__2);
	for (k = 1; k <= 2; ++k) {
	    xtry[k - 1] = xa[k - 1] + .5 + rand[k - 1] *
                          (xb[k - 1] - xa[k - 1] - d);
	}
	if (i > 1) {
/* repeat if there is overlap with existing particle */
	    i__1 = i - 1;
	    for (j = 1; j <= i__1; ++j) {
/* Computing 2nd power */
		d__1 = xtry[0] - x[(j << 1) - 2];
/* Computing 2nd power */
		d__2 = xtry[1] - x[(j << 1) - 1];
		if (d__1 * d__1 + d__2 * d__2 <= dsq) {
		    goto L10;
		}
	    }
	}
	x[(i << 1) - 2] = xtry[0];
	x[(i << 1) - 1] = xtry[1];
	xlst[(i << 1) - 2] = x[(i << 1) - 2];
	xlst[(i << 1) - 1] = x[(i << 1) - 1];
/* draw initial positions */
	for (l = 1; l <= 46; ++l) {
	    xpl[l - 1] = x[(i << 1) - 2] + rcs[l - 1];
	    ypl[l - 1] = x[(i << 1) - 1] + rsn[l - 1];
	}
	i__1 = i % 8 + 1;
	grstcl_(&i__1);
	grplin_(&c__46, xpl, ypl);
	if (i == 1) {
/* draw double circle for sphere with non zero initial velocity */
	    for (l = 1; l <= 46; ++l) {
		xpl[l - 1] = x[(i << 1) - 2] + rcs[l - 1] * .9;
		ypl[l - 1] = x[(i << 1) - 1] + rsn[l - 1] * .9;
	    }
	    grplin_(&c__46, xpl, ypl);
	}
    }
/* set initial velocities */
    for (i = 2; i <= 50; ++i) {
	v[(i << 1) - 2] = 0.;
	v[(i << 1) - 1] = 0.;
    }
    rnecuy_(rand, &c__1);
    phi = rand[0] * 6.28318;
    v[0] = cos(phi) * 1.;
    v[1] = sin(phi) * 1.;
/* loop over all times */
    i__1 = nt;
    for (m = 1; m <= i__1; ++m) {
/* loop over all particles */
	for (i = 1; i <= 50; ++i) {
/* advance particle */
	    for (k = 1; k <= 2; ++k) {
		x[k + (i << 1) - 3] += v[k + (i << 1) - 3] * .01;
	    }
/* if advance is large enough draw piece of trajectory */
	    if (iswit == 1) {
		if ((d__1 = xlst[(i << 1) - 2] - x[(i << 1) - 2], abs(d__1)) 
			> del || (d__2 = xlst[(i << 1) - 1] - x[(i << 1) - 1],
			 abs(d__2)) > del) {
		    xpl[0] = xlst[(i << 1) - 2];
		    ypl[0] = xlst[(i << 1) - 1];
		    xpl[1] = x[(i << 1) - 2];
		    ypl[1] = x[(i << 1) - 1];
		    xlst[(i << 1) - 2] = x[(i << 1) - 2];
		    xlst[(i << 1) - 1] = x[(i << 1) - 1];
		    i__2 = i % 8 + 1;
		    grstcl_(&i__2);
		    grplin_(&c__2, xpl, ypl);
		}
	    }
	}
/* test for collisions */
	for (i = 1; i <= 50; ++i) {
/* test for collisions with wall */
	    for (k = 1; k <= 2; ++k) {
		if (x[k + (i << 1) - 3] < xa[k - 1] + .5 || x[k + (i << 1) - 3]
                    > xb[k - 1] - .5) {
		    v[k + (i << 1) - 3] = -v[k + (i << 1) - 3];
		}
	    }
/* test for collisions with another particle */
	    if (i < 50) {
		for (j = i + 1; j <= 50; ++j) {
		    dx[0] = x[(i << 1) - 2] - x[(j << 1) - 2];
		    dx[1] = x[(i << 1) - 1] - x[(j << 1) - 1];
/* Computing 2nd power */
		    d__1 = dx[0];
/* Computing 2nd power */
		    d__2 = dx[1];
		    dd = d__1 * d__1 + d__2 * d__2;
		    if (dd < dsq) {
			dinv = 1. / sqrt(dd);
			eperp[0] = dx[0] * dinv;
			eperp[1] = dx[1] * dinv;
			epar[0] = eperp[1];
			epar[1] = -eperp[0];
			mtxgcl_(v, vi, &c__2, &c__50, &i);
			mtxgcl_(v, vj, &c__2, &c__50, &j);
			mtxadv_(vi, vj, w, &c__2);
			mtxmsv_(w, w, &c_dp5, &c__2);
			mtxsbv_(vi, w, ui, &c__2);
			mtxdot_(ui, eperp, &uiperp, &c__2);
			mtxdot_(ui, epar, &uipar, &c__2);
			uiperp = -uiperp;
			mtxmsv_(eperp, h1, &uiperp, &c__2);
			mtxmsv_(epar, h2, &uipar, &c__2);
			mtxadv_(h1, h2, ui, &c__2);
			mtxadv_(ui, w, vi, &c__2);
			mtxmsv_(ui, uj, &c_dm1, &c__2);
			mtxadv_(uj, w, vj, &c__2);
			mtxpcl_(v, vi, &c__2, &c__50, &i);
			mtxpcl_(v, vj, &c__2, &c__50, &j);
		    }
		}
	    }
	}
	if (iswit == 2) {
	    if (m % mt == 1 || m == nt) {
/* produce histogram */
                sprintf(capt + 4, "%5li", m);
                capt[9] = ' ';
		smhsin_(hist, &x0, &delx, &nx);
		for (j = 1; j <= 50; ++j) {
		    if (ihist == 1) {
			his = v[(j << 1) - 2];
		    }
		    if (ihist == 2) {
			his = v[(j << 1) - 1];
		    }
		    if (ihist == 3) {
/* Computing 2nd power */
			d__1 = v[(j << 1) - 2];
/* Computing 2nd power */
			d__2 = v[(j << 1) - 1];
			his = sqrt(d__1 * d__1 + d__2 * d__2);
		    }
		    if (ihist == 4) {
/* Computing 2nd power */
			d__1 = v[(j << 1) - 2];
			his = d__1 * d__1 + v[(j << 1) - 1];
		    }
		    smhsfl_(hist, &x0, &delx, &nx, &his, &c_d1);
		}
		smhsgr_(hist, &x0, &delx, &nx, tx, &c__15, ty, &c__15, capt,
			 &c__15, &nws);
	    }
	}
    }
    grclse_();
    return 0;
} /* main */

