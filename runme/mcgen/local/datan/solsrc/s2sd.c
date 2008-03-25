#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__1000 = 1000;
static double c_dp999 = .999, c_dmp15 = -.15, c_dp9 = .9, c_dp15 = .15,
              c_dp85 = .85, c_dmp414 = -.414, c_d1 = 1., c_d0 = 0.;

main(void)
{
    /* Local variables */
    static integer i, n, iswit1, iswit, lca, ltx, lty, nws, nf1, nf2;
    static double del, xpl[1000], ypl[1000], sigma, x0, xmin, ymin, xmax, ymax;
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    double d__1;
/* identify program to user */
    printf("%s\n", "Program S2D provides interactively graphs of");
    printf("%s\n\n", "statistical functions of a continuous variable.");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
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
    printf("%s", "> ");
    scanf("%li", &iswit1);
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    if (iswit == 1) {
/* standardized normal */
        xmin = -3.;
        xmax = 3.;
        ymin = 0.;
        if (iswit1 == 1) {
            ymax = .5;
        } else {
            ymax = 1.;
        }
        strncpy(tx, "x", 1);
        ltx = 1;
        if (iswit1 == 1) {
            strncpy(ty, "&f@(x)", 6);
        } else if (iswit1 == 2) {
            strncpy(ty, "&F@(x)", 6);
        }
        lty = 6;
        strncpy(capt, "standardized normal distribution", 32);
        lca = 32;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            if (iswit1 == 1) {
                ypl[i - 1] = sdstnr_(&xpl[i - 1]);
            } else if (iswit1 == 2) {
                ypl[i - 1] = scstnr_(&xpl[i - 1]);
            }
        }
    } else if (iswit == 2) {
/* normal */
        printf("%s\n", "Enter x0:");
        printf("%s", "> ");
        scanf("%lf", &x0);
        printf("%s\n", "Enter sigma (> 0.):");
        printf("%s", "> ");
        scanf("%lf", &sigma);
        xmin = sigma * -3. + x0;
        xmax = sigma * 3. + x0;
        ymin = 0.;
        if (iswit1 == 1) {
            ymax = sdnorm_(&x0, &x0, &sigma) * 1.2;
        } else {
            ymax = 1.;
        }
        strncpy(tx, "x", 1);
        ltx = 1;
        if (iswit1 == 1) {
            strncpy(ty, "f(x)", 4);
        } else if (iswit1 == 2) {
            strncpy(ty, "F(x)", 4);
        }
        lty = 4;
        sprintf(capt, "normal distribution with x_0# = %6.2lf, &s@ = %6.2lf",
                x0, sigma);
        capt[52]= ' ';
        lca = 52;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            if (iswit1 == 1) {
                ypl[i - 1] = sdnorm_(&xpl[i - 1], &x0, &sigma);
            } else if (iswit1 == 2) {
                ypl[i - 1] = scnorm_(&xpl[i - 1], &x0, &sigma);
            }
        }
    } else if (iswit == 3) {
/* chi-squared */
        printf("%s\n", "Enter n (>= 1):");
        printf("%s", "> ");
        scanf("%li", &n);
        xmin = 0.;
        xmax = sqchi2_(&c_dp999, &n);
        if (n == 1) {
            xmin = 1e-5;
        }
        ymin = 0.;
        if (iswit1 == 1) {
            if (n == 1) {
                ymax = 2.;
            }
            if (n == 2) {
                ymax = .5;
            }
            if (n > 2) {
                d__1 = (double) (n - 1);
                ymax = sdchi2_(&d__1, &n) * 1.2;
            }
        } else if (iswit1 == 2) {
            ymax = 1.;
        }
        strncpy(tx, "&c^2", 4);
        ltx = 4;
        if (iswit1 == 1) {
            strncpy(ty, "f(&c@^2#)", 9);
        } else if (iswit1 == 2) {
            strncpy(ty, "F(&c@^2#)", 9);
        }
        lty = 9;
        sprintf(capt, "&c@^2# distribution with n = %3li", n);
        capt[32] = ' ';
        lca = 32;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            if (iswit1 == 1) {
                ypl[i - 1] = sdchi2_(&xpl[i - 1], &n);
            } else if (iswit1 == 2) {
                ypl[i - 1] = scchi2_(&xpl[i - 1], &n);
            }
        }
    } else if (iswit == 4) {
/* F distribution */
        printf("%s\n", "Enter f1 (>= 1):");
        printf("%s", "> ");
        scanf("%li", &nf1);
        printf("%s\n", "Enter f2 (>= 1):");
        printf("%s", "> ");
        scanf("%li", &nf2);
        xmin = 0.;
        xmax = 3.;
        ymin = 0.;
        ymax = 1.;
        strncpy(tx, "F", 1);
        ltx = 1;
        if (iswit1 == 1) {
            strncpy(ty, "f(F)", 4);
        } else if (iswit1 == 2) {
            strncpy(ty, "F(F)", 4);
        }
        lty = 4;
        sprintf(capt, "F distribution with f_1# = %3li, f_2# = %3li",
                nf1, nf2);
        capt[42] = ' ';
        lca = 42;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            if (iswit1 == 1) {
                ypl[i - 1] = sdftst_(&xpl[i - 1], &nf1, &nf2);
            } else if (iswit1 == 2) {
                ypl[i - 1] = scftst_(&xpl[i - 1], &nf1, &nf2);
            }
        }
    } else if (iswit == 5) {
/* t distribution */
        printf("%s\n", "Enter n (>=1):");
        printf("%s", "> ");
        scanf("%li", &n);
        xmin = -3.;
        xmax = 3.;
        ymin = 0.;
        if (iswit1 == 1) {
            ymax = .5;
        } else {
            ymax = 1.;
        }
        strncpy(tx, "t", 1);
        ltx = 1;
        if (iswit1 == 1) {
            strncpy(ty, "f(t)", 4);
        } else if (iswit1 == 2) {
            strncpy(ty, "F(t)", 4);
        }
        lty = 4;
        sprintf(capt, "t distribution with n = %3li", n);
        capt[27] = ' ';
        lca = 27;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            if (iswit1 == 1) {
                ypl[i - 1] = sdstud_(&xpl[i - 1], &n);
            } else if (iswit1 == 2) {
                ypl[i - 1] = scstud_(&xpl[i - 1], &n);
            }
        }
    }
    (void)getchar();
/* graphics */
    gropen_();
    gropws_(&nws);
    grwncc_(&xmin, &xmax, &ymin, &ymax);
    grvwwc_(&c_dmp15, &c_dp9, &c_dp15, &c_dp85);
    grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
    grfram_();
    grboun_();
    grsclx_(tx, &ltx);
    grscly_(ty, &lty);
    grtxtc_(&c_d1, capt, &lca);
    grstcl_(&c__2);
    grplin_(&c__1000, xpl, ypl);
    grclse_();
    return 0;
} /* main */

