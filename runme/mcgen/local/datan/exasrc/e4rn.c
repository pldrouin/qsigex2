#include <stdio.h>
#include <math.h>
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__50 = 50;
static double c_d1 = 1., c_dp5 = .5, c_dm1 = -1.;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer i, j, k, m, nws, nt = 5000, mt = 500, nx = 40;
    static double xa[2] = { 0.,0. }, xb[2] = { 10.,10. }, x0 = -1., delx = .05,
                  rand[2], epar[2], dinv, hist[40], xtry[2], d,
                  v[100] /* was [2][50] */, w[2], x[100] /* was [2][50] */,
                  eperp[2], uipar, h1[2], h2[2], dd, dx[2], ui[2],
                  vi[2], vj[2], uj[2], delphi, uiperp, dsq, phi;
    /* System generated locals */
    integer i__1;
    double d__1, d__2;
/* identify program to user */
    printf("%s\n", "Program E4RN performs");
    printf("%s\n\n", "Monte Carlo simulation of molecular motion.");
/* initialization */
    d = 1.;
    dsq = d * d;
    delphi = .1396262222222222;
    nws = 1;
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
/* write header for output table */
    printf("%s\n", "The following lines are numerical");
    printf("%s\n", "histograms for the x-component of velocity");
    printf("%s\n", "velocity of the 50 particles in the simulation.");
    printf("%s\n", "Each line is a histogram for a fixed time");
    printf("%s\n", "Line number 1 shows that 49 particles are at");
    printf("%s\n", "rest. They are in the histogram bin around");
    printf("%s\n", "vx=0. One particle has nonzero velocity.");
/* loop over all times */
    i__1 = nt;
    for (m = 1; m <= i__1; ++m) {
	if (m % mt == 1) {
/* produce histogram */
	    smhsin_(hist, &x0, &delx, &nx);
	    for (j = 1; j <= 50; ++j) {
		smhsfl_(hist, &x0, &delx, &nx, &v[(j << 1) - 2], &c_d1);
	    }
            for (k = 1; k <= 40; ++k)
                printf("%2li", (integer)hist[k-1]);
            printf("\n");
	}
/* advance all particles */
	for (i = 1; i <= 50; ++i) {
	    for (k = 1; k <= 2; ++k) {
		x[k + (i << 1) - 3] += v[k + (i << 1) - 3] * .01;
	    }
	}
/* test for collisions */
	for (i = 1; i <= 50; ++i) {
/* test for collisions with wall */
	    for (k = 1; k <= 2; ++k) {
		if (x[k + (i << 1) - 3] <= xa[k - 1] + .5 ||
                    x[k + (i << 1) - 3] >= xb[k - 1] - .5) {
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
		    if (dd <= dsq) {
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
    }
    return 0;
} /* main */

