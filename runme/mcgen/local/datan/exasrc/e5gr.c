#include <stdio.h>
#include <string.h>
#include "grsrc.h"
#include "datsrc.h"

/* Table of constant values */

static integer c__0 = 0, c__1 = 1, c__2 = 2, c__3 = 3, c__5 = 5, c__6 = 6,
               c__9 = 9, c__75 = 75, c__201 = 201;
static double c_dm10 = -10., c_d10 = 10., c_dmp1 = -.1, c_dp5 = .5, c_d0 = 0., 
              c_d1p414 = 1.414, c_d1 = 1., c_d5 = 5., c_dp2 = .2, c_dp8 = .8;
static logical c_true = TRUE_, c_false = FALSE_;

main()
{
    /* Local variables */
    static integer i, iplot, nws;
    static double delx, x0, xa, xb, ya, yb, tx[201], ty[201];
    static char textx[75], texty[75];

/* identify program to user */
    (void)printf("%s\n", "Program E5GR demonstrates use of");
    (void)printf("%s\n\n", "GRSCDF and GRCCRS.");
/* ask for number of workstation */
    grnbws_();
    (void)printf("%s\n", "Please, enter number of workstation:");
    (void)printf("%s", "> ");
    (void)scanf("%ld", &nws);
    (void)getchar();
/* prepare texts for scales */
    (void)memset(textx, ' ', 75);
    (void)memset(texty, ' ', 75);
    (void)strncpy(textx, "x", 1);
    (void)strncpy(textx, "f(x)", 4);
    gropen_();
    gropws_(&nws);
    grwncc_(&c_dm10, &c_d10, &c_dmp1, &c_dp5);
    grwnwc_(&c_d0, &c_d1p414, &c_d0, &c_d1);
    grfram_();
/* loop over 4 plots */
    for (iplot = 1; iplot <= 4; ++iplot) {
/* set view port in WC for different plots */
	xa = (float).25;
	if (iplot == 2 || iplot == 4) {
	    xa = .957;
	}
	xb = xa + (float).4;
	ya = (float).65;
	if (iplot == 3 || iplot == 4) {
	    ya = (float).15;
	}
	yb = ya + (float).3;
	grvwwc_(&xa, &xb, &ya, &yb);
	grstcl_(&c__1);
/* draw boundary (around window in CC) */
	grboun_();
/* draw coordinate cross */
	grccrs_();
	if (iplot == 1) {
/* plot 1 (upper left): take defaults for designing scales */
	    grsclx_(textx, &c__75);
	    grscly_(texty, &c__75);
	} else if (iplot == 2) {
/* plot 2 (upper right): */
/* change design of scales */
	    grscdf_(&c_d0, &c_d5, &c__0, &c_d1, &c_d1, &c_true);
	    grsclx_(textx, &c__75);
	    grscdf_(&c_d0, &c_dp2, &c__2, &c_d0, &c_d0, &c_true);
	    grscly_(texty, &c__75);
	} else if (iplot == 3) {
/* plot 2 (lower left): */
/* change design of scales */
	    grscdf_(&c_d0, &c_d5, &c__0, &c_dp8, &c_dp8, &c_true);
	    grsclx_(textx, &c__75);
	    grscdf_(&c_d0, &c_dp2, &c__2, &c_dp8, &c_dp8, &c_true);
	    grscly_(texty, &c__75);
	} else if (iplot == 4) {
/* plot 2 (lower right right): */
/* change design of scales */
	    grscdf_(&c_d0, &c_d5, &c__5, &c_dp8, &c_dp8, &c_false);
	    grsclx_(textx, &c__75);
	    grscdf_(&c_d0, &c_dp2, &c__1, &c_dp8, &c_dp8, &c_false);
	    grscly_(texty, &c__75);
	}
/* prepare and draw polyline */
	delx = .1;
	x0 = -10.;
	for (i = 1; i <= 201; ++i) {
	    tx[i - 1] = x0 + (i - 1) * delx;
	    ty[i - 1] = sdstnr_(&tx[i - 1]);
	}
	grstcl_(&c__6);
	grplin_(&c__201, tx, ty);
    }
    grclse_();
    return 0;
} /* main */

