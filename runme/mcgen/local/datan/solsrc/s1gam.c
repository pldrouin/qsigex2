#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__1 = 1, c__2 = 2;
static double c_dmp15 = -.15, c_dp9 = .9, c_dp15 = .15, c_dp85 = .85,
              c_dmp414 = -.414, c_d1 = 1., c_d0 = 0.;

main(void)
{
    /* Local variables */
    static integer i, iswit, ltx, lty, nws, npl;
    static double del, x1, x2, xa, xb, xmin, ymin, xmax, ymax,
                  xpl[1000], ypl[1000];
    static char tx[75], ty[75];
    /* System generated locals */
    integer i__1;
    double d__1;
/* identify program to user */
    printf("%s\n", "Program S1GAM provides interactively graphs");
    printf("%s\n\n", "of the gamma function and its inverse.");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Choose:");
    printf("%s\n", "1 - gamma function");
    printf("%s\n", "2 - 1/gamma");
    printf("%s", "> ");
    scanf("%li", &iswit);
    (void)getchar();
    xmin = -5.;
    xmax = 5.;
    ymin = -5.;
    ymax = 5.;
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    strncpy(tx, "x", 1);
    ltx = 1;
    if (iswit == 1) {
        strncpy(ty, "&G@(x)", 6);
        lty = 6;
    } else if (iswit == 2) {
        strncpy(ty, "1/&G@(x)", 8);
        lty = 8;
    }
    gropen_();
    gropws_(&nws);
    grwncc_(&xmin, &xmax, &ymin, &ymax);
    grvwwc_(&c_dmp15, &c_dp9, &c_dp15, &c_dp85);
    grwnwc_(&c_dmp414, &c_d1, &c_d0, &c_d1);
    grfram_();
    grboun_();
    grccrs_();
    grsclx_(tx, &ltx);
    grscly_(ty, &lty);
    grstcl_(&c__2);
    if (iswit == 1) {
/* gamma */
        d__1 = xmin - .5;
        xa = (double) nint(d__1);
L5:
        xb = xa + 1.;
        if (xa < 0.) {
/* draw separate polylines between adjacent poles for x<0 */
            x1 = xa + 1e-5;
            x2 = xb - 1e-5;
            npl = 100;
            del = (x2 - x1) / (double) (npl - 1);
            i__1 = npl;
            for (i = 1; i <= i__1; ++i) {
                xpl[i - 1] = x1 + (i - 1) * del;
                ypl[i - 1] = ggamma_(&xpl[i - 1]);
            }
            grplin_(&npl, xpl, ypl);
            xa += 1.;
            goto L5;
        } else {
/* draw one polyline for x>0 */
            xa += 1e-5;
            npl = 1000;
            del = (xmax - xa) / (double) (npl - 1);
            i__1 = npl;
            for (i = 1; i <= i__1; ++i) {
                xpl[i - 1] = xa + (i - 1) * del;
                ypl[i - 1] = ggamma_(&xpl[i - 1]);
            }
            grplin_(&npl, xpl, ypl);
        }
    } else if (iswit == 2) {
/* 1/gamma */
        npl = 1000;
        del = (xmax - xmin) / (double) (npl - 1);
        i__1 = npl;
        for (i = 1; i <= i__1; ++i) {
            xpl[i - 1] = xmin + (i - 1) * del;
            ypl[i - 1] = 1. / ggamma_(&xpl[i - 1]);
        }
        grplin_(&npl, xpl, ypl);
    }
    grclse_();
    return 0;
} /* main */

