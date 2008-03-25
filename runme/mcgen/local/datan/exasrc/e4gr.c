#include <stdio.h>
#include <math.h>
#include <string.h>
#include "grsrc.h"

extern double CBFUNC contfn(double const P_T *, double const P_T *);

/* Table of constant values */

static integer c__1 = 1, c__3 = 3, c__9 = 9, c__75 = 75;
static double c_dm3p14159 = -3.14159, c_d3p14159 = 3.14159, c_d0 = 0., 
              c_d1 = 1., c_dmp621 = -.621, c_d1p5 = 1.5, c_dmp3 = -.3, 
              c_d1p2 = 1.2;

main()
{
    /* Local variables */
    static integer i, nx, ny, nws;
    static double cont, d, x0, y0, dx, dy, delcnt;
    static char string[75];

/* identify program to user */
    (void)printf("%s\n\n", "Program E4GR demonstrates use of GRPLCT.");
/* ask for number of workstation */
    grnbws_();
    (void)printf("%s\n", "Please, enter number of workstation:");
    (void)printf("%s", "> ");
    (void)scanf("%ld", &nws);
    (void)getchar();
/* initialization */
    gropen_();
    gropws_(&nws);
    grwncc_(&c_dm3p14159, &c_d3p14159, &c_dm3p14159, &c_d3p14159);
    grvwwc_(&c_d0, &c_d1, &c_d0, &c_d1);
    grwnwc_(&c_dmp621, &c_d1p5, &c_dmp3, &c_d1p2);
    grstcl_(&c__1);
    grfram_();
    grboun_();
/* scales */
    (void)memset(string, ' ', 75);
    string[0] = 'x';
    grsclx_(string, &c__75);
    string[0] = 'y';
    grscly_(string, &c__75);
    (void)strncpy(string, "sin(x+y)cos((x-y)/2)", 20);
    grtxtc_(&c_d1p2, string, &c__75);
/* define pixels */
    d = .064;
    x0 = -3.2;
    y0 = -3.2;
    dx = d;
    dy = d;
    nx = 100;
    ny = 100;
    grstcl_(&c__3);
/* plot contour lines */
    delcnt = .2;
    for (i = 1; i <= 9; ++i) {
	cont = 1. - (double) i * delcnt;
	grplct_(&x0, &y0, &dx, &dy, &nx, &ny, &cont, contfn);
    }
    grclse_();
    return 0;
} /* main */

/* ********************************************************** */
double CBFUNC contfn(double const P_T *x, double const P_T *y)
{
    /* System generated locals */
    double ret_val;

/* function for which contour lines are to be drawn */
    ret_val = sin(*x + *y) * cos((*x - *y) * .5);
    return ret_val;
} /* contfn */
