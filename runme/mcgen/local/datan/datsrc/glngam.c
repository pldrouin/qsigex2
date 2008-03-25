#include <math.h>
#include "datsrc.h"

#if defined(MAKELIB)
extern struct {
   double c[6];
} cmgamm;
#endif

double LIBFUNC glngam_(double const P_T *x)
{
    /* Initialized data */
#if !defined(MAKELIB)
    static double c[6] = { 76.18009173,-86.50532033,24.01409822,
            -1.231739516,.00120858003,-5.36382e-6 };
#else
    double *c = cmgamm.c;
#endif

    /* System generated locals */
    double ret_val;

    /* Local variables */
    integer i;
    double anum, g, s, xh, xx, xgh;
    logical reflec;

    if (*x >= 1.) {
        reflec = FALSE_;
        xx = *x - 1.;
    } else {
        reflec = TRUE_;
        xx = 1. - *x;
    }
    xh = xx + .5;
    xgh = xx + 5.5;
    s = 1.;
    anum = xx;
    for (i = 1; i <= 6; ++i) {
        anum += 1.;
        s += c[i - 1] / anum;
    }
    s *= 2.506628275;
    g = xh * log(xgh) + log(s) - xgh;
    if (reflec) {
        ret_val = log(xx * 3.141592654) - g - log(sin(xx * 3.141592654));
    } else {
        ret_val = g;
    }
    return ret_val;
} /* glngam_ */

