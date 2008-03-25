#include <stdio.h>
#include "datsrc.h"

main(void)
{
    static integer i, n, iseed1, iseed2;
    static double a[2], c[4] /* was [2][2] */, x[2], dplus[4] /* was [2][2] */;
    static char answer[3];
/* identify program to user */
    printf("%s\n", "Program E5RN demonstrates use ");
    printf("%s\n\n", "of RNMNPR, RNMNGN.");
/* show current seeds */
    rne2ot_(&iseed1, &iseed2);
    printf("%s%10li%10li\n", "Current seeds are ", iseed1, iseed2);
/* ask for new seed(s) */
    printf("%s\n", "Do you want to enter new seed(s) (y/n)?");
    printf("%s", "> ");
    scanf("%1s", answer);
    if (*answer == 'Y' || *answer == 'y') {
/* read new seeds and initiate generator */
        printf("%s\n", "Enter two integers (>0) as seeds:");
        printf("%s", "> ");
        scanf("%li%li", &iseed1, &iseed2);
	rne2in_(&iseed1, &iseed2);
    }
/* ask for mean and covariance matrix */
    printf("%s\n", "Pairs of correlated random numbers");
    printf("%s\n", "will be generated.");
    printf("%s\n", "Enter element C11 (>0.) of covariance matrix");
    printf("%s\n", "(variance of first number in pair):");
    printf("%s", "> ");
    scanf("%lf", &c[0]);
    printf("%s\n", "Enter element C22 (>0.)of covariance matrix");
    printf("%s\n", "(variance of second number in pair):");
    printf("%s", "> ");
    scanf("%lf", &c[3]);
    printf("%s\n", "Enter element C12 of covariance matrix");
    printf("%s\n", "(covariance of the two numbers)");
    printf("%s\n", "(C12=C11*C22*rho, -1.<rho<1.):");
    printf("%s", "> ");
    scanf("%lf", &c[2]);
    c[1] = c[2];
    printf("%s\n", "Enter mean value for first number:");
    printf("%s", "> ");
    scanf("%lf", &a[0]);
    printf("%s\n", "Enter mean value for second number:");
    printf("%s", "> ");
    scanf("%lf", &a[1]);
/* initiate generator for multivariate normally distributes numbers */
    n = 2;
    rnmnpr_(c, dplus, &n);
/* generate and write random numbers */
    printf("%s\n", "Pairs of random numbers are");
    for (i = 1; i <= 20; ++i) {
	rnmngn_(dplus, a, x, &n);
        printf("%20.15lG%20.15lG\n", x[0], x[1]);
    }
    return 0;
} /* main */

