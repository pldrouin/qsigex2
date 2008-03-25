#include "datsrc.h"

void LIBFUNC auxzbr_(double P_T *x0, double P_T *x1,
             double (CBFUNC P_T *funct)(double const P_T *,
             double const P_T *, integer const P_T *, integer const P_T *),
             double const P_T *par, integer const P_T *npar1,
             integer const P_T *npar2)
{
    integer i;
    double f0, f1, xs;

    if (*x0 == *x1) {
        *x1 = *x0 + 1.;
    }
    f0 = (*funct)(x0, par, npar1, npar2);
    f1 = (*funct)(x1, par, npar1, npar2);
    for (i = 1; i <= 1000; ++i) {
        if (f0 * f1 > 0.) {
            if (abs(f0) <= abs(f1)) {
                xs = *x0;
                *x0 += (*x0 - *x1) * 2.;
                *x1 = xs;
                f1 = f0;
                f0 = (*funct)(x0, par, npar1, npar2);
            } else {
                xs = *x1;
                *x1 += (*x1 - *x0) * 2.;
                *x0 = xs;
                f0 = f1;
                f1 = (*funct)(x1, par, npar1, npar2);
            }
        } else {
            goto L20;
        }
    }
L20:
    return;
} /* auxzbr_ */

