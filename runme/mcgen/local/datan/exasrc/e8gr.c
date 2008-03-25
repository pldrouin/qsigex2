#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grsrc.h"
#include "datsrc.h"


/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__9 = 9, c__1000 = 1000;
static double c_d0 = 0.;

main()
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer ndat, ncol[3], i, k, lcapt, nmark, iswit, ncurve, nws,
                   ltx, lty;
    static double sigma, *xpl /* was [1000][3] */, *ypl /* was [1000][3] */;
    static double delx, datx[21], daty[21], datsx[21], datsy[21], datcov[21],
                  x0, scalef;
    static char capt[75], tx[75], ty[75];

    
    if ((xpl = (double *)MEMALLOC(sizeof(double)*3000)) == NULL ||
        (ypl = (double *)MEMALLOC(sizeof(double)*3000)) == NULL) {
      MEMERROR("e8gr");
      return(0);
    }
/* COMMON /CME8GR/ is needed for Microsoft FORTRAN compiler */
/* identify program to user */
    (void)printf("%s\n\n", "Program E8GR demonstrates use of GRDTMC.");
/* ask for number of workstation */
    grnbws_();
    (void)printf("%s\n", "Please, enter number of workstation:");
    (void)printf("%s", "> ");
    (void)scanf("%ld", &nws);
/* ask for type of display */
    (void)printf("%s\n", "Choose:");
    (void)printf("%s\n", "1 - display data points only");
    (void)printf("%s\n", "2 - display curves only");
    (void)printf("%s\n", "3 - display both data points and curves");
    (void)printf("%s", "> ");
    (void)scanf("%ld", &iswit);
    (void)getchar();
/* set number NDAT of data points and number NCURVE of polylines */
    if (iswit == 1) {
	ndat = 21;
	ncurve = 0;
    } else if (iswit == 2) {
	ndat = 0;
	ncurve = 3;
    } else if (iswit == 3) {
	ndat = 21;
	ncurve = 3;
    }
/* generate simulated data points */
    if (ndat > 0) {
	rnstnr_(daty, &ndat);
	i__1 = ndat;
	for (i = 1; i <= i__1; ++i) {
	    datx[i - 1] = (i - 1) * .3 - 3.;
	    daty[i - 1] = daty[i - 1] * .05 + sdstnr_(&datx[i - 1]);
	    datsx[i - 1] = 0.;
	    datsy[i - 1] = .05;
	    datcov[i - 1] = 0.;
	}
    }
/* compute points on polylines */
    if (ncurve > 0) {
	x0 = -10.;
	delx = abs(x0) * 2. / 999.;
	i__1 = ncurve;
	for (k = 1; k <= i__1; ++k) {
	    sigma = (double) k * .5;
	    ncol[k - 1] = k + 2;
	    if (k != 2) {
		ncol[k - 1] = -ncol[k - 1];
	    }
	    for (i = 1; i <= 1000; ++i) {
		xpl[i + k * 1000 - 1001] = x0 + (i - 1) * delx;
		ypl[i + k * 1000 - 1001] = sdnorm_(&xpl[i + 
			k * 1000 - 1001], &c_d0, &sigma);
	    }
	}
    }
/* prepare texts and captions */
    (void)memset(tx, ' ', 75);
    (void)memset(ty, ' ', 75);
    (void)memset(capt, ' ', 75);
    (void)strncpy(tx, "x", 1);
    ltx = 1;
    (void)strncpy(ty, "f(x)", 4);
    lty = 4;
    if (iswit == 1) {
	(void)strncpy(capt, "Data points", 11);
	lcapt = 11;
    } else if (iswit == 2) {
	(void)strncpy(capt, "Gaussians of different widths", 29);
	lcapt = 29;
    } else if (iswit == 3) {
	(void)strncpy(capt, "Data points and Gaussians of different widths", 45);
	lcapt = 45;
    }
    nmark = 5;
    scalef = .5;
/* generate graphics by call of GRDTMC */
    grdtmc_(xpl, ypl, &c__1000, &ncurve, ncol, &nmark, &scalef,
           datx, daty, datsx, datsy, datcov, &ndat, tx, &ltx, ty, &lty,
           capt, &lcapt, &nws);
    return 0;
} /* main */

