#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__5 = 5, c__27 = 27;
static double c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, j, k, n, nint, nf, idelnf, nx, nf0, nws;
    static double delf, delx, hist[1000], f[1000], r, f0, x0;
    static char capt[28], tx[5], ty[5];
    /* System generated locals */
    integer i__1, i__2;
/* identify program to user */
    printf("%s\n", "Program E2DS simulates experiment");
    printf("%s\n", "of Rutherford and Geiger");
    printf("%s\n\n", "on statistical nature of radioactive decays.");
/* ask for numerical input */
    printf("%s\n", "Enter number N of observed decays:");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter number NINT of time intervals (1 <= NINT <= 1000):");
    printf("%s", "> ");
    scanf("%li", &nint);
/* prepare histogram */
    x0 = 0.;
    delx = 1.;
    nx = nint;
    nf = (integer) ((double) n / (double) nint);
    idelnf = (integer) sqrt((double) nf) + 1;
/* Computing MAX */
    i__1 = 0, i__2 = nf - idelnf * 5;
    nf0 = max(i__1,i__2);
    nf += idelnf * 5;
    f0 = -.5;
    delf = 1.;
    nf = min(nf, 1000);
/* prepare texts */
    sprintf(capt, "N = %6li, n_int# = %6li", n, nint);
    smhsin_(hist, &x0, &delx, &nx);
/* simulate decay times and fill histogram */
    i__1 = n;
    for (i = 1; i <= i__1; ++i) {
	rnecuy_(&r, &c__1);
	r *= (double) nint;
	smhsfl_(hist, &x0, &delx, &nx, &r, &c_d1);
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
    strncpy(tx, "t", 1);
    strncpy(ty, "N(t)", 4);
    smhsgr_(hist, &x0, &delx, &nx, tx, &c__5, ty, &c__5, capt, &c__27, &nws);
/* analyze histogram of decay times */
    i__1 = nf + 1;
    for (i = 1; i <= i__1; ++i) {
	k = i - 1;
	f[i - 1] = 0.;
	i__2 = nx;
	for (j = 1; j <= i__2; ++j) {
	    if ((integer) hist[j - 1] == k) {
		f[i - 1] += 1.;
	    }
	}
    }
    strncpy(tx, "k", 1);
    strncpy(ty, "N(k)", 4);
    smhsgr_(f, &f0, &delf, &nf, tx, &c__5, ty, &c__5, capt, &c__27, &nws);
    return 0;
} /* main */

