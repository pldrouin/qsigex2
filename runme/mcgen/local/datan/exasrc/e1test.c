#include <stdio.h>
#include <string.h>
#include "datsrc.h"

main(void)
{
    /* Local variables */
    static integer i, iexp, nexp, n1, n2, ng, nk;
    static double dels, sgsq, sksq, dels2, c, f, q, s, xmean, delxm,
                  r1[1000], r2[1000], sigma1, sigma2, confid, s1sq, s2sq;
    static char text[15];
    /* System generated locals */
    integer i__1, i__2, i__3;
/* identify program to user */
    printf("%s\n", "Program E1TEST simulates 2 Gaussian samples");
    printf("%s\n\n", "and performs F-test for equality of variances.");
/* ask for input */
    printf("%s\n", "Enter number NEXP of experiments (>0):");
    printf("%s", "> ");
    scanf("%li", &nexp);
    printf("%s\n",
           "Enter number N1 of elements in 1st sample (1 <= N1 <= 1000):");
    printf("%s", "> ");
    scanf("%li", &n1);
    printf("%s\n",
           "Enter number N2 of elements in 2nd sample (1 <= N2 <= 1000):");
    printf("%s", "> ");
    scanf("%li", &n2);
    printf("%s\n", "Enter standard dev. SIGMA1 for in 1st sample (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma1);
    printf("%s\n", "Enter standard dev. SIGMA2 for in 2nd sample (>0.):");
    printf("%s", "> ");
    scanf("%lf", &sigma2);
    printf("%s\n", "Enter confidence limit CONFID (0.<CONFID<1.):");
    printf("%s", "> ");
    scanf("%lf", &confid);
    c = (confid + 1.) * .5;
/* write heading for table */
    printf("%s\n", "IEXP   NG   NK     SGSQ        SKSQ        F           Q");
/* loop over all experiments */
    i__1 = nexp;
    for (iexp = 1; iexp <= i__1; ++iexp) {
/* simulate samples R1 and R2 */
        rnstnr_(r1, &n1);
        i__2 = n1;
        for (i = 1; i <= i__2; ++i) {
            r1[i - 1] *= sigma1;
        }
        rnstnr_(r2, &n2);
        i__2 = n2;
        for (i = 1; i <= i__2; ++i) {
            r2[i - 1] *= sigma2;
        }
/* compute sample variances and F quotient */
        smmnvr_(r1, &n1, &xmean, &delxm, &s1sq, &dels2, &s, &dels);
        smmnvr_(r2, &n2, &xmean, &delxm, &s2sq, &dels2, &s, &dels);
        if (s1sq > s2sq) {
            sgsq = s1sq;
            sksq = s2sq;
            ng = n1;
            nk = n2;
        } else {
            sksq = s1sq;
            sgsq = s2sq;
            nk = n1;
            ng = n2;
        }
        f = sgsq / sksq;
        i__2 = ng - 1;
        i__3 = nk - 1;
        q = sqftst_(&c, &i__2, &i__3);
        if (f > q) {
            strcpy(text, " F-test failed");
        } else {
            strcpy(text, "              ");
        }
/* output */
        printf("%5li%5li%5li%12.5lf%12.5lf%12.5lf%12.5lf%s\n",
               iexp, ng, nk, sgsq, sksq, f, q, text);
    }
    return 0;
} /* main */

