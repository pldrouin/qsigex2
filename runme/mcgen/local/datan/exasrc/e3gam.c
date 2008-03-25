#include <stdio.h>
#include "datsrc.h"

main(void)
{
    static integer iswit;
    static double a, b, f, w, x, z;
/* identify program to user */
    printf("%s\n", "Program E3GAM demonstrates use of");
    printf("%s\n", "GBETAF,GINCGM and GINCBT");
    printf("%s\n", "and provides interactively numerical values");
    printf("%s\n", "of the beta function, the incomplete gamma");
    printf("%s\n\n", "function, and the incomplete beta function.");
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - beta function");
    printf("%s\n", "2 - incomplete gamma function");
    printf("%s\n", "3 - incomplete beta function");
    printf("%s", "> ");
    scanf("%li", &iswit);
    if (iswit == 1) {
/* beta */
        printf("%s\n", "Enter z (> 0.):");
        printf("%s", "> ");
        scanf("%lf", &z);
        printf("%s\n", "Enter w (> 0.):");
        printf("%s", "> ");
        scanf("%lf", &w);
        f = gbetaf_(&z, &w);
        printf("%s%lf\n", "beta(z,w) = ", f);
    } else if (iswit == 2) {
/* incomplete gamma */
        printf("%s\n", "Enter a (>0.):");
        printf("%s", "> ");
        scanf("%lf", &a);
        printf("%s\n", "Enter x (>0.):");
        printf("%s", "> ");
        scanf("%lf", &x);
        f = gincgm_(&a, &x);
        printf("%s%lf\n", "P(a,x) = ", f);
    } else if (iswit == 3) {
/* incomplete beta */
        printf("%s\n", "Enter a, (0.<a<1.):");
        printf("%s", "> ");
        scanf("%lf", &a);
        printf("%s\n", "Enter b, (0.<b<1.):");
        printf("%s", "> ");
        scanf("%lf", &b);
        printf("%s\n", "Enter x, (0.<x<1.):");
        printf("%s", "> ");
        scanf("%lf", &x);
        f = gincbt_(&a, &b, &x);
        printf("%s%lf\n", "I_x (a,b) = ", f);
    }
    return 0;
} /* main */

