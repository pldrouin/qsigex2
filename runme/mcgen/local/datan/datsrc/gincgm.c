#include <math.h>
#include "datsrc.h"

double LIBFUNC gincgm_(double const P_T *a, double const P_T *x)
{
    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */
    integer i, j;
    double help, anum, f, s, cfnew, a0, b0, a1, b1, fnorm, cf, aloggm,
           a2j, b2j, a2j1, b2j1;

    aloggm = glngam_(a);
    if (*x <= *a + 1.) {
/* series development */
        f = 1. / *a;
        s = f;
        anum = *a;
        for (i = 1; i <= 100; ++i) {
            anum += 1.;
            f = *x * f / anum;
            s += f;
            if (f < 1e-6) {
                goto L20;
            }
        }
L20:
        if (*x < 1e-6) {
            ret_val = 0.;
        } else {
            help = *a * log(*x) - *x - aloggm;
            if (abs(help) >= 500.) {
                ret_val = 0.;
            } else {
                ret_val = s * exp(help);
            }
        }
    } else {
/* continued fraction */
        a0 = 0.;
        b0 = 1.;
        a1 = 1.;
        b1 = *x;
        cf = 1.;
        fnorm = 1.;
        for (j = 1; j <= 100; ++j) {
            a2j = (double) j - *a;
            a2j1 = (double) j;
            b2j = 1.;
            b2j1 = *x;
            a0 = (b2j * a1 + a2j * a0) * fnorm;
            b0 = (b2j * b1 + a2j * b0) * fnorm;
            a1 = b2j1 * a0 + a2j1 * a1 * fnorm;
            b1 = b2j1 * b0 + a2j1 * b1 * fnorm;
            if (b1 != 0.) {
/* renormalize and test for convergence */
                fnorm = 1. / b1;
                cfnew = a1 * fnorm;
                if ((d__1 = cf - cfnew, abs(d__1)) / cf < 1e-6) {
                    goto L40;
                }
                cf = cfnew;
            }
        }
L40:
        help = *a * log(*x) - *x - aloggm;
        if (abs(help) >= 500.) {
            ret_val = 1.;
        } else {
            ret_val = 1. - exp(help) * cfnew;
        }
    }
    return ret_val;
} /* gincgm_ */

