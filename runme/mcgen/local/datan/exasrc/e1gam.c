#include <stdio.h>
#include "datsrc.h"

main(void)
{
    static integer iswit;
    static double f, x;
/* identify program to user */
    printf("%s\n", "Program E1GAM demonstrates use of");
    printf("%s\n", "GGAMMA and GLNGAM");
    printf("%s\n", "and provides interactively numerical values");
    printf("%s\n\n", "of the gamma function and its logarithm.");
    printf("%s\n", "Choose");
    printf("%s\n", "1 - gamma function");
    printf("%s\n", "2 - log of gamma function");
    printf("%s", "> ");
    scanf("%li", &iswit);
    printf("%s\n", "Enter x:");
    printf("%s", "> ");
    scanf("%lf", &x);
    if (iswit == 1) {
/* gamma */
	f = ggamma_(&x);
        printf("%s%14.10lf\n", "gamma(x) = ", f);
    } else if (iswit == 2) {
/* log gamma */
	f = glngam_(&x);
        printf("%s%14.10lf\n", "log (gamma(x)) = ", f);
    }
    return 0;
} /* main */

