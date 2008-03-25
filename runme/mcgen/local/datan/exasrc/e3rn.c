#include <stdio.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__100 = 100;
static double c_d1 = 1.;

main(void)
{
    /* Local variables */
    static integer i, n, iswit, nws;
    static double delx, hist[100], a, t, tau1, tau2, x0;
/* identify program to user */
    printf("%s\n\n", "Program E3RN demonstrates use of RNRADI.");
/* ask for numerical input */
    printf("%s\n", "Enter number of decays N (>>1):");
    printf("%s", "> ");
    scanf("%li", &n);
    printf("%s\n", "Enter fraction A (0. < A < 1.):");
    printf("%s", "> ");
    scanf("%lf", &a);
    printf("%s\n", "Enter TAU1 (>0.):");
    printf("%s", "> ");
    scanf("%lf", &tau1);
    printf("%s\n", "Enter TAU2 (>0.):");
    printf("%s", "> ");
    scanf("%lf", &tau2);
/* ask for type of output */
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - Numerical Output");
    printf("%s\n", "2 - Graphical Output");
    printf("%s", "> ");
    scanf("%li", &iswit);
    if (iswit == 1) {
/* numerical output */
        printf("%s%6.2lf%s%6.2lf%s%6.2lf\n",
               "A = ", a, ", TAU1=", tau1, ", TAU2 = ", tau2);
        printf("%s\n", "Decay time values produced by RNRADI:");
	for (i = 1; i <= n; ++i) {
/* simulate decay times */
	    rnradi_(&a, &tau1, &tau2, &t);
            printf("%12.7lf\n ", t);
	}
    } else if (iswit == 2) {
/* graphical output */
        grnbws_();
        printf("%s\n", "Please, enter number of workstation:");
        printf("%s", "> ");
        scanf("%li", &nws);
        (void)getchar();
/* initiate and fill histogram */
	x0 = 0.;
	delx = max(tau1,tau2) * .03;
	smhsin_(hist, &x0, &delx, &c__100);
	for (i = 1; i <= n; ++i) {
/* simulate decay times */
	    rnradi_(&a, &tau1, &tau2, &t);
	    smhsfl_(hist, &x0, &delx, &c__100, &t, &c_d1);
	}
/* present histogram */
	smhsgr_(hist, &x0, &delx, &c__100, "t", &c__1, "N", &c__1, " ", &c__1,
	       &nws);
    }
    return 0;
} /* main */

