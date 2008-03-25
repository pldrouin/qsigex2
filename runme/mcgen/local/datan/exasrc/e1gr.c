#include <stdio.h>
#include <string.h>
#include "grsrc.h"
#include "datsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__6 = 6, c__9 = 9,
               c__75 = 75, c__201 = 201;
static double c_d0 = 0., c_d1 = 1., c_d10 = 10., c_d1p35 = 1.35,
              c_d1p414 = 1.414, c_d1p5 = 1.5, c_d2 = 2., c_d3 = 3., c_d5 = 5.,
              c_dm10 = -10., c_dm4p5 = -4.5, c_dp2 = .2, c_dp25 = .25,
              c_dp3 = .3, c_dp4 = .4, c_dp5 = .5, c_dp8 = .8;

main()
{
    /* Local variables */
    static double delx;
    static integer i;
    static double x0, tx[201], ty[201];
    static char string[75];
    static integer nws;

/* identify program to user */
    printf("%s\n", "Program E1GR demonstrates use of");
    printf("%s\n", "GROPEN,GRCLSE,GROPWS,GRCLWS,GRWNCC,GRVWWC,");
    printf("%s\n", "GRWNWC,GRSTFR,GRFRAM,GRBOUN,GRSTCL,GRPLIN,");
    printf("%s\n", "GRBRPL,GRSCLX,GRSCLY,GRTXTF.");
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%ld", &nws);
    (void)getchar();
/* open GRPACK */
    gropen_();
/* open workstation */
    gropws_(&nws);
/* define window in computing coordinates (CC) */
    grwncc_(&c_dm10, &c_d10, &c_d0, &c_dp5);
/* define view port in world coordinates (WC) */
    grvwwc_(&c_dp25, &c_d1p35, &c_dp2, &c_dp8);
/* define window in WC */
    grwnwc_(&c_d0, &c_d1p414, &c_d0, &c_d1);
/* define format in device coordinates */
    grstfr_(&nws, &c_d5, &c_d0);
/* draw frame (given by window in WC) */
    grfram_();
/* draw boundary (around window in CC) */
    grboun_();
/* scale on abscissa */
    (void)memset(string, ' ', 75);
    (void)strncpy(string, "x", 1);
    grsclx_(string, &c__1);
/* scale on ordinate */
    (void)strncpy(string, "f(x)", 4);
    grscly_(string, &c__4);
/* caption */
    (void)strncpy(string, "f(x)=(2&ps^2#@)^-1/2#exp(-(x-a)^2#/2&s^2#@)", 43);
    grtxtc_(&c_d1p5, string, &c__75);
/* compute points for polyline of standardized normal */
    delx = .1;
    x0 = -10.;
    for (i = 1; i <= 201; ++i) {
	tx[i - 1] = x0 + (i - 1) * delx;
	ty[i - 1] = sdstnr_(&tx[i - 1]);
    }
/* set color index */
    grstcl_(&c__6);
/* draw continuous polyline */
    grplin_(&c__201, tx, ty);
/* compute points for polyline of Gaussian with mean 2 */
/* and standard deviation 3 */
    for (i = 1; i <= 201; ++i) {
	tx[i - 1] = x0 + (i - 1) * delx;
	ty[i - 1] = sdnorm_(&tx[i - 1], &c_d2, &c_d3);
    }
/* draw broken polyline */
    grbrpl_(&c__1, &c_d0, &c__201, tx, ty);
/* compute and draw short straight continuous polyline */
    tx[0] = -8.;
    tx[1] = -5.;
    ty[0] = .4;
    ty[1] = .4;
    grplin_(&c__2, tx, ty);
/* place text to the right of short polyline */
    (void)memset(string, ' ', 75);
    (void)strncpy(string, "a=0, &s=1", 9);
    grtxtf_(&c_dm4p5, &c_dp4, &c_d1, string, &c__75);
/* compute and draw short straight broken polyline */
    ty[0] = .3f;
    ty[1] = .3f;
    grbrpl_(&c__1, &c_d0, &c__2, tx, ty);
/* place text to its right */
    (void)strncpy(string, "a=2, &s=3", 9);
    grtxtf_(&c_dm4p5, &c_dp3, &c_d1, string, &c__75);
/* close workstation */
    grclws_(&nws);
/* close GRPACK */
    grclse_();
    return 0;
} /* main */

