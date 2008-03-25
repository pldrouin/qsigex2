#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__5 = 5, c__40 = 40;
static double c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer nexp, i, j, k, n, nx, nws;
    static double delx, hist[1001], p, r[1000], x0;
    static char tx[5], ty[5], capt[40];
    /* System generated locals */
    integer i__1, i__2;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program E3DS simulates Galton's board");
    printf("%s\n\n", "and demonstrates statistical fluctuations.");
/* ask for numerical input */
    printf("%s\n", "Enter number NEXP of trials:");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n", "Enter number n of steps per trial (n<1001):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter probability p:");
    printf("%s", "> ");
    scanf("%lf", &p);
/* initialization */
    sprintf(capt, "p = %5.3lf, n = %4li, N_exp# = %7li", p, n, nexp);
    x0 = -.5;
    delx = 1.;
    nx = n + 1;
    smhsin_(hist, &x0, &delx, &nx);
/* simulate trials and fill histogram */
    i__1 = nexp;
    for (i = 1; i <= i__1; ++i) {
	rnecuy_(r, &n);
	k = 0;
	if (n > 0) {
	    i__2 = n;
	    for (j = 1; j <= i__2; ++j) {
		if (r[j - 1] < p) {
		    ++k;
		}
	    }
	}
	d__1 = (double) k;
	smhsfl_(hist, &x0, &delx, &nx, &d__1, &c_d1);
    }
/* graphical output */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    (void)getchar();
/* present histogram */
    memset(tx, ' ', 5);
    memset(ty, ' ', 5);
    strncpy(tx, "k", 1);
    strncpy(ty, "N(k)", 4);
    smhsgr_(hist, &x0, &delx, &nx, tx, &c__5, ty, &c__5, capt, &c__40, &nws);
    return 0;
} /* main */

