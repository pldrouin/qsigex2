#include <stdio.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2, c__1000 = 1000;
static double c_dmp15 = -.15, c_dp9 = .9, c_dp15 = .15, c_dp85 = .85,
              c_dmp414 = -.414, c_d1 = 1., c_d0 = 0.;

main(void)
{
    /* Local variables */
    static integer i, iswit, lca, ltx, lty, nws;
    static double del, xpl[1000], ypl[1000], xmin, ymin, xmax, ymax, a, b, z;
    static char capt[75], tx[75], ty[75];
/* identify program to user */
    printf("%s\n", "Program S2GAM provides interactively graphs");
    printf("%s\n", "of the beta function the incomplete");
    printf("%s\n\n", "gamma function and incomplete beta function.");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - beta function");
    printf("%s\n", "2 - incomplete gamma function");
    printf("%s\n", "3 - incomplete beta function");
    printf("%s", "> ");
    scanf("%li", &iswit);
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    if (iswit == 1) {
/* beta */
        printf("%s\n", "Enter z (> 0.):");
        printf("%s", "> ");
        scanf("%lf", &z);
        xmin = 0.;
        xmax = 2.;
        ymin = 0.;
        ymax = 5.;
        strncpy(tx, "w", 1);
        ltx = 1;
        strncpy(ty, "B(z,w)", 6);
        lty = 6;
        sprintf(capt, " z = %6.3lf", z);
        capt[11] = ' ';
        lca = 11;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            ypl[i - 1] = gbetaf_(&z, &xpl[i - 1]);
        }
    } else if (iswit == 2) {
/* incomplete gamma */
        printf("%s\n", "Enter a (> 0.):");
        printf("%s", "> ");
        scanf("%lf", &a);
        xmin = 0.;
        xmax = 10.;
        ymin = 0.;
        ymax = 1.;
        strncpy(tx, "x", 1);
        ltx = 1;
        strncpy(ty, "P(a,x)", 6);
        lty = 6;
        sprintf(capt, " a = %6.3lf", a);
        capt[11] = ' ';
        lca = 11;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            ypl[i - 1] = gincgm_(&a, &xpl[i - 1]);
        }
    } else if (iswit == 3) {
/* incomplete beta */
        printf("%s\n", "Enter a (> 0.):");
        printf("%s", "> ");
        scanf("%lf", &a);
        printf("%s\n", "Enter b (> 0.):");
        printf("%s", "> ");
        scanf("%lf", &b);
        xmin = 0.;
        xmax = 1.;
        ymin = 0.;
        ymax = 1.;
        strncpy(tx, "w", 1);
        ltx = 1;
        strncpy(ty, "I_x#(a,b)", 9);
        lty = 9;
        sprintf(capt, " a = %6.3lf, b = %6.3lf", a, b);
        capt[23] = ' ';
        lca = 23;
        del = (xmax - xmin) / 999.;
        for (i = 1; i <= 1000; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            ypl[i - 1] = gincbt_(&a, &b, &xpl[i - 1]);
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

