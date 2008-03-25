#include <stdio.h>
#include "datsrc.h"

main(void)
{
    static integer k, n, iswit, iswit1, kk, nn;
    static double f, p, alambd;
/* identify program to user */
    printf("%s\n", "Program E1SD demonstrates use of");
    printf("%s\n", "SDBINM,SCBINM,SDHYPG,SCHYPG,");
    printf("%s\n", "SDPOIS,SCPOIS,SQPOIS");
    printf("%s\n", "and provides interactively numerical values");
    printf("%s\n\n", "of statistical functions of a discrete variable.");
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - binomial distribution");
    printf("%s\n", "2 - hypergeometric distribution");
    printf("%s\n", "3 - Poisson distribution");
    printf("%s", "> ");
    scanf("%li", &iswit);
    if (iswit == 1) {
/* binomial */
        printf("%s\n", "Choose:");
        printf("%s\n", "1 - probability");
        printf("%s\n", "2 - distribution function (cumulative prob.)");
        printf("%s", "> ");
        scanf("%li", &iswit1);
        if (iswit1 == 1) {
            printf("%s\n", "Enter p (0.<p<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
            printf("%s\n", "Enter n (>=0):");
            printf("%s", "> ");
            scanf("%li", &n);
            printf("%s\n", "Enter k (0<=k<=n):");
            printf("%s", "> ");
            scanf("%li", &k);
	    f = sdbinm_(&k, &n, &p);
            printf("%s%14.10lf\n", "W(n,k) = ", f);
	} else if (iswit1 == 2) {
            printf("%s\n", "Enter p (0.<p<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
            printf("%s\n", "Enter n (>=0):");
            printf("%s", "> ");
            scanf("%li", &n);
            printf("%s\n", "Enter K (0<=k<=n):");
            printf("%s", "> ");
            scanf("%li", &k);
	    f = scbinm_(&k, &n, &p);
            printf("%s%14.10lf\n", "P(k<K) = ", f);
	}
    } else if (iswit == 2) {
        printf("%s\n", "Choose:");
        printf("%s\n", "1 - probability");
        printf("%s\n", "2 - distribution function (cumulative prob.)");
        printf("%s", "> ");
        scanf("%li", &iswit1);
	if (iswit1 == 1) {
            printf("%s\n", "Enter K (>=0):");
            printf("%s", "> ");
            scanf("%li", &kk);
            printf("%s\n", "Enter N (>=K):");
            printf("%s", "> ");
            scanf("%li", &nn);
            printf("%s\n", "Enter k (0<=k<=K):");
            printf("%s", "> ");
            scanf("%li", &k);
            printf("%s\n", "Enter n (k<=n<N):");
            printf("%s", "> ");
            scanf("%li", &n);
	    f = sdhypg_(&k, &n, &kk, &nn);
            printf("%s%14.10lf\n", "W(k) = ", f);
	} else if (iswit1 == 2) {
            printf("%s\n", "Enter K (>=0):");
            printf("%s", "> ");
            scanf("%li", &kk);
            printf("%s\n", "Enter N (>=K):");
            printf("%s", "> ");
            scanf("%li", &nn);
            printf("%s\n", "Enter kprime (<=K):");
            printf("%s", "> ");
            scanf("%li", &k);
            printf("%s\n", "Enter n (<=N):");
            printf("%s", "> ");
            scanf("%li", &n);
	    f = schypg_(&k, &n, &kk, &nn);
            printf("%s%14.10lf\n", "P(k<kprime) = ", f);
	}
    } else if (iswit == 3) {
        printf("%s\n", "Choose:");
        printf("%s\n", "1 - probability");
        printf("%s\n", "2 - distribution function (cumulative prob.)");
        printf("%s\n", "3 - quantile");
        printf("%s\n", ">");
        scanf("%li", &iswit1);
	if (iswit1 == 1) {
            printf("%s\n", "Enter k (>=0):");
            printf("%s", "> ");
            scanf("%li", &k);
            printf("%s\n", "Enter lambda (>0.):");
            printf("%s", "> ");
            scanf("%lf", &alambd);
	    f = sdpois_(&k, &alambd);
            printf("%s%14.10lf\n", "f(k;lambda) = ", f);
	} else if (iswit1 == 2) {
            printf("%s\n", "Enter K (>=0):");
            printf("%s", "> ");
            scanf("%li", &kk);
            printf("%s\n", "Enter lambda (>0.):");
            printf("%s", "> ");
            scanf("%lf", &alambd);
	    f = scpois_(&kk, &alambd);
            printf("%s%14.10lf\n", "P(k<K) = ", f);
	} else if (iswit1 == 3) {
            printf("%s\n", "Enter K (>=0):");
            printf("%s", "> ");
            scanf("%li", &kk);
            printf("%s\n", "Enter P (0.<P<1.):");
            printf("%s", "> ");
            scanf("%lf", &p);
	    f = sqpois_(&kk, &p);
            printf("%s%14.10lf\n", "lambda_P (K) = ", f);
	}
    }
    return 0;
} /* main */

