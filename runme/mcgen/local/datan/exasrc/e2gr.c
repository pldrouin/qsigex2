#include <stdio.h>
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__9 = 9;
static double c_dm5 = -5., c_d5 = 5., c_d0 = 0., c_dp5 = .5, c_d1p4 = 1.4, 
              c_d1 = 1., c_d1p414 = 1.414;

main()
{
    /* Local variables */
    static integer n, nws;
    static double s, x, y, dx, yy;
    static char string[1];

/* identify program to user */
    (void)printf("%s\n\n", "Program E2GR demonstrates use of GRMARK.");
/* ask for number of workstation */
    grnbws_();
    (void)printf("%s\n", "Please, enter number of workstation:");
    (void)printf("%s", "> ");
    (void)scanf("%li", &nws);
    (void)getchar();
/* initialization */
    gropen_();
    gropws_(&nws);
    grwncc_(&c_dm5, &c_d5, &c_d0, &c_dp5);
    grvwwc_(&c_d0, &c_d1p4, &c_d0, &c_d1);
    grwnwc_(&c_d0, &c_d1p414, &c_d0, &c_d1);
    grfram_();
    n = 1;
    s = 1.;
    x = -5.;
    dx = 1.;
/* loop over different types of mark */
    for (n = 1; n <= 9; ++n) {
	x += dx;
	if (n % 2 != 0) {
	    y = .35;
	    yy = .4;
	} else {
	    y = .15;
	    yy = .1;
	}
/* prepare text string to identify marks */
        string[0] = (char)(n + '0');
/* draw mark */
	grmark_(&n, &s, &x, &y);
/* place text near mark */
	grtxtf_(&x, &yy, &s, string, &c__1);
    }
    grclse_();
    return 0;
} /* main */
