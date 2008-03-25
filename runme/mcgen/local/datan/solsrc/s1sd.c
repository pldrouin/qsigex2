#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__4 = 4, c__6 = 6, c__7 = 7,
               c__12 = 12, c__28 = 28;

main(void)
{
    /* Local variables */
    static integer iswit1, iswit, k, lcapt, n, kk, nn, nx, kmax, nws;
    static double alambd, delx, hist[1000], f, p, x0;
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program S1SD provides interactively graphs of");
    printf("%s\n\n", "statistical functions of a discrete variable.");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - binomial distribution");
    printf("%s\n", "2 - hypergeometric distribution");
    printf("%s\n", "3 - Poisson distribution");
    printf("%s", "> ");
    scanf("%li", &iswit);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - probability");
    printf("%s\n", "2 - distribution function (cumulative prob.)");
    printf("%s", "> ");
    scanf("%li", &iswit1);
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    if (iswit == 1) {
/* binomial */
        printf("%s\n", "Enter p (0. <= p <= 1.):");
        printf("%s", "> ");
        scanf("%lf", &p);
        printf("%s\n", "Enter n (0 <= n <= 998):");
        printf("%s", "> ");
        scanf("%li", &n);
        (void)getchar();
        delx = 1.;
        if (iswit1 == 1) {
            strncpy(tx, "k", 1);
            strncpy(ty, "P(k)", 4);
            x0 = -.5;
            nx = n + 1;
        } else if (iswit1 == 2) {
            strncpy(tx, "K", 1);
            strncpy(ty, "P(k<K)", 6);
            x0 = -1.;
            nx = n + 2;
        }
        smhsin_(hist, &x0, &delx, &nx);
        sprintf(capt, "n = %4li", n);
        i__1 = n;
        for (k = 0; k <= i__1 + 1; ++k) {
            if (iswit1 == 1) {
                hist[k] = sdbinm_(&k, &n, &p);
            } else if (iswit1 == 2) {
                hist[k] = scbinm_(&k, &n, &p);
            }
        }
        lcapt = strlen(capt);
        smhsgr_(hist, &x0, &delx, &nx, tx, &c__1, ty, &c__6, capt, &lcapt,&nws);
     } else if (iswit == 2) {
/* hypergeometric */
        printf("%s\n", "Enter K (0 <= K <= 998):");
        printf("%s", "> ");
        scanf("%li", &kk);
        printf("%s\n", "Enter N (>= K):");
        printf("%s", "> ");
        scanf("%li", &nn);
        printf("%s\n", "Enter n (0 <= n <= N):");
        printf("%s", "> ");
        scanf("%li", &n);
        (void)getchar();
        kmax = min(n,kk);
        delx = 1.;
        nx = kmax + 2;
        sprintf(capt, "K = %4li, N = %4li, n = %4li", kk, nn, n);
        if (iswit1 == 1) {
            strncpy(tx, "k", 1);
            strncpy(ty, "P(k)", 4);
            x0 = -.5;
        } else if (iswit1 == 2) {
            strncpy(tx, "k'", 2);
            strncpy(ty, "P(k<k')", 7);
            x0 = -1.;
        }
        smhsin_(hist, &x0, &delx, &nx);
        i__1 = kmax;
        for (k = 0; k <= i__1 + 1;  ++k) {
            if (iswit1 == 1) {
                hist[k] = sdhypg_(&k, &n, &kk, &nn);
            } else if (iswit1 == 2) {
                hist[k] = schypg_(&k, &n, &kk, &nn);
            }
        }
        lcapt = strlen(capt);
        smhsgr_(hist, &x0, &delx, &nx, tx, &c__2, ty, &c__7, capt, &lcapt, &nws);
     } else if (iswit == 3) {
/* Poisson */
        printf("%s\n", "Enter lambda (0. < lambda < 10):");
        printf("%s", "> ");
        scanf("%lf", &alambd);
        (void)getchar();
        f = sdpois_(&k, &alambd);
        d__1 = alambd * 10.;
        kmax = nint(d__1);
        delx = 1.;
        nx = kmax + 1;
        sprintf(capt, "&l@ = %6.2lf", alambd);
        if (iswit1 == 1) {
            strncpy(tx, "k", 1);
            strncpy(ty, "P(k)", 4);
            x0 = -.5;
        } else if (iswit1 == 2) {
            strncpy(tx, "K", 1);
            strncpy(ty, "P(k<K)", 6);
            x0 = -1.;
        }
        smhsin_(hist, &x0, &delx, &nx);
        i__1 = kmax;
        for (k = 0; k <= i__1; ++k) {
            if (iswit1 == 1) {
                hist[k] = sdpois_(&k, &alambd);
            } else if (iswit1 == 2) {
                hist[k] = scpois_(&k, &alambd);
            }
        }
        lcapt=strlen(capt);
        smhsgr_(hist, &x0, &delx, &nx, tx, &c__1, ty, &c__6, capt, &lcapt,&nws);
    }
    return 0;
} /* main */

