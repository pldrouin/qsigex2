#include <stdio.h>
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__3 = 3, c__4 = 4, c__9 = 9, c__5 = 5;
static double c_dm1p2 = -1.2, c_d1 = 1., c_d0 = 0., c_d1p4 = 1.4, 
              c_d1p414 = 1.414, c_dmp8 = -.8, c_dp5 = .5, c_dp3 = .3, 
              c_dp2 = .2, c_dp7 = .7, c_dmp6 = -.6, c_dmp5 = -.5, c_dp4 = .4, 
              c_dmp4 = -.4, c_dp45 = .45, c_dp6 = .6;

main()
{
    /* Local variables */
    static double s;
    static integer nws;

/* identify program to user */
    (void)printf("%s\n\n", "Program E3GR demonstrates use of GRDATP.");
/* ask for number of workstation */
    grnbws_();
    (void)printf("%s\n", "Please, enter number of workstation:");
    (void)printf("%s", "> ");
    (void)scanf("%ld", &nws);
    (void)getchar();
/* initialization */
    gropen_();
    gropws_(&nws);
    grwncc_(&c_dm1p2, &c_d1, &c_dm1p2, &c_d1);
    grvwwc_(&c_d0, &c_d1p4, &c_d0, &c_d1);
    grwnwc_(&c_d0, &c_d1p414, &c_d0, &c_d1);
    grfram_();
/* draw different data points */
    s = 1.;
    grdatp_(&c__1, &s, &c_dmp8, &c_dp5, &c_dp3, &c_d0, &c_d0);
    grdatp_(&c__2, &s, &c_d0, &c_dp5, &c_d0, &c_dp2, &c_d0);
    grdatp_(&c__3, &s, &c_dp7, &c_dp5, &c_dp2, &c_dp3, &c_d0);
    grdatp_(&c__4, &s, &c_dmp6, &c_dmp5, &c_dp4, &c_dp4, &c_dmp4);
    grdatp_(&c__5, &s, &c_dp5, &c_dmp5, &c_dp45, &c_dp3, &c_dp6);
    grclse_();
    return 0;
} /* main */

