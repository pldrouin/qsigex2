#include <math.h>
#include "datsrc.h"

double LIBFUNC gincbt_(double const P_T *aa, double const P_T *bb,
               double const P_T *xx)
{
    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    integer m;
    double d2m, d2m1, xlim, apl2m, a, b, x, cfnew, fnorm, a1, b1, a2, b2,
           cf, rm;
    logical reflec;

    xlim = (*aa + 1.) / (*aa + *bb + 1.);
    if (*xx < xlim) {
        reflec = FALSE_;
        a = *aa;
        b = *bb;
        x = *xx;
    } else {
        reflec = TRUE_;
        a = *bb;
        b = *aa;
        x = 1. - *xx;
    }
    if (x < 1e-8) {
/* function known at end of range */
        cf = 0.f;
    } else {
/* continued fraction */
        a1 = 1.;
        b1 = 1.;
        a2 = 1.;
        b2 = 1. - (a + b) * x / (a + 1.);
        fnorm = 1. / b2;
        cf = a2 * fnorm;
        for (m = 1; m <= 100; ++m) {
            rm = (double) m;
            apl2m = a + rm * 2.;
            d2m = rm * (b - rm) * x / ((apl2m - 1.) * apl2m);
            d2m1 = -(a + rm) * (a + b + rm) * x / (apl2m * (apl2m + 1));
            a1 = (a2 + d2m * a1) * fnorm;
            b1 = (b2 + d2m * b1) * fnorm;
            a2 = a1 + d2m1 * a2 * fnorm;
            b2 = b1 + d2m1 * b2 * fnorm;
            if (b2 != 0.f) {
/* renormalize and test for convergence */
                fnorm = 1. / b2;
                cfnew = a2 * fnorm;
                if ((d__1 = cf - cfnew, abs(d__1)) / cf < 1e-8) {
                    goto L20;
                }
                cf = cfnew;
            }
        }
L20:
        d__1 = 1. - x;
        cf = cf * pow(x, a) * pow(d__1, b) / (a * gbetaf_(&a, &b));
    }
    if (reflec) {
        ret_val = 1. - cf;
    } else {
        ret_val = cf;
    }
    return ret_val;
} /* gincbt_ */

