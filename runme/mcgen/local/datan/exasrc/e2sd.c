#include <stdio.h>
#include "datsrc.h"

main(void)
{
    static integer n, iswit, iswit1, nf1, nf2;
    static double f, p, t, x, sigma, x0, xf;
/* identify program to user */
    printf("%s\n", "Program E2SD demonstrates use of");
    printf("%s\n", "SDSTNR,SCSTNR,SQSTNR,SDNORM,SCNORM,SQNORM,");
    printf("%s\n", "SDCHI2,SCCHI2,SQCHI2,SDFTST,SCFTST,SQFTST,");
    printf("%s\n", "SDSTUD,SCSTUD,SQSTUD");
    printf("%s\n", "and provides interactively numerical values");
    printf("%s\n\n", "of statistical functions of a continuous variable.");
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - standardized normal distribution");
    printf("%s\n", "2 - normal distribution");
    printf("%s\n", "3 - chi-squared distribution");
    printf("%s\n", "4 - F distribution");
    printf("%s\n", "5 - t distribution");
    printf("%s", "> ");
    scanf("%li", &iswit);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - probability density");
    printf("%s\n", "2 - distribution function");
    printf("%s\n", "3 - quantile");
    printf("%s", "> ");
    scanf("%li", &iswit1);
    if (iswit == 1) {
/* standardized normal */
	if (iswit1 <= 2) {
            printf("%s\n", "Enter x (>= 0.):");
            printf("%s", "> ");
            scanf("%lf", &x);
	    if (iswit1 == 1) {
		f = sdstnr_(&x);
                printf("%s%14.10lf\n", "f(x) = ", f);
	    } else if (iswit1 == 2) {
		f = scstnr_(&x);
                printf("%s%14.10lf\n", "F(x) = ", f);
	    }
	} else if (iswit1 == 3) {
            printf("%s\n", "Enter P (0.<P<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
	    f = sqstnr_(&p);
            printf("%s%14.10lf\n", "x_P = ", f);
	}
    } else if (iswit == 2) {
/* normal */
	if (iswit1 <= 2) {
            printf("%s\n", "Enter x0:");
            printf("%s", "> ");
            scanf("%lf", &x0);
            printf("%s\n", "Enter sigma (>0.):");
            printf("%s", "> ");
            scanf("%lf", &sigma);
            printf("%s\n", "Enter x:");
            printf("%s", "> ");
            scanf("%lf", &x);
	    if (iswit1 == 1) {
		f = sdnorm_(&x, &x0, &sigma);
                printf("%s%14.10lf\n", "f(x;x0,sigma) = ", f);
	    } else if (iswit1 == 2) {
		f = scnorm_(&x, &x0, &sigma);
                printf("%s%14.10lf\n", "F(x;x0,sigma) = ", f);
	    }
	} else if (iswit1 == 3) {
            printf("%s\n", "Enter x0:");
            printf("%s", "> ");
            scanf("%lf", &x0);
            printf("%s\n", "Enter sigma (>0.):");
            printf("%s", "> ");
            scanf("%lf", &sigma);
            printf("%s\n", "Enter P (0.<P<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
	    f = sqnorm_(&p, &x0, &sigma);
            printf("%s%14.10lf\n", "x_P (x0,sigma)= ", f);
	}
    } else if (iswit == 3) {
/* chi-squared */
	if (iswit1 <= 2) {
            printf("%s\n", "Enter x:");
            printf("%s", "> ");
            scanf("%lf", &x);
            printf("%s\n", "Enter n (>= 1):");
            printf("%s", "> ");
            scanf("%li", &n);
	    if (iswit1 == 1) {
		f = sdchi2_(&x, &n);
                printf("%s%14.10lf\n", "f(chi-squared;n) = ", f);
	    } else if (iswit1 == 2) {
		f = scchi2_(&x, &n);
                printf("%s%14.10lf\n", "F(chi-squared;n) = ", f);
	    }
	} else if (iswit1 == 3) {
            printf("%s\n", "Enter n (>= 1):");
            printf("%s", "> ");
            scanf("%li", &n);
            printf("%s\n", "Enter P (0.<P<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
	    f = sqchi2_(&p, &n);
            printf("%s%14.10lf\n", "chi-squared_P (n)= ", f);
	}
    } else if (iswit == 4) {
/* F distribution */
	if (iswit1 <= 2) {
            printf("%s\n", "Enter F (>0.):");
            printf("%s", "> ");
            scanf("%lf", &xf);
            printf("%s\n", "Enter f1 (>= 1):");
            printf("%s", "> ");
            scanf("%li", &nf1);
            printf("%s\n", "Enter f2 (>= 1):");
            printf("%s", "> ");
            scanf("%li", &nf2);
	    if (iswit1 == 1) {
		f = sdftst_(&xf, &nf1, &nf2);
                printf("%s%14.10lf\n", "f(F;f1,f2) = ", f);
	    } else if (iswit1 == 2) {
		f = scftst_(&xf, &nf1, &nf2);
                printf("%s%14.10lf\n", "F(F;f1,f2) = ", f);
	    }
	} else if (iswit1 == 3) {
            printf("%s\n", "Enter f1 (>= 1):");
            printf("%s", "> ");
            scanf("%li", &nf1);
            printf("%s\n", "Enter f2 (>= 1):");
            printf("%s", "> ");
            scanf("%li", &nf2);
            printf("%s\n", "Enter P (0.<P<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
	    f = sqftst_(&p, &nf1, &nf2);
            printf("%s%14.10lf\n", "F_P (f1,f2)= ", f);
	}
    } else if (iswit == 5) {
/* t distribution */
	if (iswit1 <= 2) {
            printf("%s\n", "Enter t:");
            printf("%s", "> ");
            scanf("%lf", &t);
            printf("%s\n", "Enter n (>= 1):");
            printf("%s", "> ");
            scanf("%li", &n);
	    if (iswit1 == 1) {
		f = sdstud_(&t, &n);
                printf("%s%14.10lf\n", "f(t;n) = ", f);
	    } else if (iswit1 == 2) {
		f = scstud_(&t, &n);
                printf("%s%14.10lf\n", "F(t;n) = ", f);
	    }
	} else if (iswit1 == 3) {
            printf("%s\n", "Enter n (>= 1):");
            printf("%s", "> ");
            scanf("%li", &n);
            printf("%s\n", "Enter P (0.<P<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
	    f = sqstud_(&p, &n);
            printf("%s%14.10lf\n", "t_P (n)= ", f);
	}
    }
    return 0;
} /* main */

