#include <stdio.h>
#include <math.h>
#include <string.h>
#include "datsrc.h"
#include "grsrc.h"

/* Table of constant values */

static integer c__10 = 10, c__200 = 200;

struct {
    double xpl[2000] /* was [200][10] */, ypl[2000] /* was [200][10] */;
} LIBDATA e1rcom;

main(void)
{
    /* Initialized data */ /* Local variables */
    static integer i, j, k, l, ncol[10], npol, lcapt, nmark, ltx, lty, nws;
    static double t[10] = { -.9,-.7,-.5,-.3,-.1,.1,.3,.5,.7,.9 },
                  y[10] = { 81.,50.,35.,27.,26.,60.,106.,189.,318.,520. },
                  a[100] /* was [10][10] */, b[100] /* was [10][10] */,
                  d, x[10], datsx[10], t0, dt, scalef, datcov[10],
                  deltay[10], chi2[10];
    static char capt[75], tx[75], ty[75];
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
/* identify program to user */
    printf("%s\n", "Program E2REG performs polynomial regression");
    printf("%s\n\n", "and presents data and polynomials graphically.");
/* set errors */
    for (i = 1; i <= 10; ++i) {
        deltay[i - 1] = sqrt(y[i - 1]);
    }
/* perform polynomial regression */
    regpol_(t, y, deltay, &c__10, &c__10, x, b, a, chi2);
/* output results in alphanumeric form */
    printf("%s\n", "T");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", t[i-1]);
    printf("\n");
    printf("%s\n", "Y");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", y[i-1]);
    printf("\n");
    printf("%s\n", "DELTAY");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", deltay[i-1]);
    printf("\n");
    printf("%s\n", "X");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", x[i-1]);
    printf("\n");
    printf("%s\n", "CHI2");
    for (i = 1; i <= 10; ++i)
        printf("%7.2f", chi2[i-1]);
    printf("\n");
/* ask for number of workstation */
    grnbws_();
    printf("%s\n", "Please, enter number of workstation:");
    printf("%s", "> ");
    scanf("%li", &nws);
    printf("%s\n", "Enter maximum number of terms in polynomials");
    printf("%s\n", "to be plotted (>0, <11):");
    printf("%s", "> ");
    scanf("%li", &npol);
    (void)getchar();
/* set errors along abscissa and covariances to zero for graphics */
    for (i = 1; i <= 10; ++i) {
        datsx[i - 1] = 0.;
        datcov[i - 1] = 0.;
    }
/* compute polylines for polynomials of degrees 0,1,2,.... */
    t0 = -1.5;
    dt = .01507537688442211;
    for (i = 1; i <= 200; ++i) {
        i__1 = npol;
        for (j = 1; j <= i__1; ++j) {
            e1rcom.xpl[i + j * 200 - 201] = t0 + (i - 1) * dt;
            e1rcom.ypl[i + j * 200 - 201] = 0.;
            i__2 = j;
            for (l = 1; l <= i__2; ++l) {
                d = b[l - 1];
                if (l > 1) {
                    i__3 = l;
                    for (k = 2; k <= i__3; ++k) {
                        i__4 = k - 1;
                        d += b[l + k * 10 - 11] *
                             pow(e1rcom.xpl[i + j * 200 - 201], (double)i__4);
                    }
                }
                e1rcom.ypl[i + j * 200 - 201] += x[l - 1] * d;
            }
        }
    }
    for (j = 1; j <= 10; ++j) {
        ncol[j - 1] = 4;
    }
/* prepare texts for graphics */
    memset(tx, ' ', 75);
    memset(ty, ' ', 75);
    memset(capt, ' ', 75);
    tx[0] = 't';
    ty[0] = 'y';
    ltx = 1;
    lty = 1;
    lcapt = 1;
    nmark = 5;
    scalef = .5;
/* call graphics routine for data points and multiple polylines */
    grdtmc_(e1rcom.xpl, e1rcom.ypl, &c__200, &npol, ncol, &nmark, &scalef,
           t, y, datsx, deltay, datcov, &c__10, tx, &ltx, ty, &lty, capt,
           &lcapt, &nws);
    return 0;
} /* main */

