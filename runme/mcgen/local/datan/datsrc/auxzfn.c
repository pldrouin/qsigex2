#include "datsrc.h"

void LIBFUNC auxzfn_(double P_T *x0, double P_T *x1, double P_T *xzero,
             double (CBFUNC P_T *funct)(double const P_T *,
             double const P_T *, integer const P_T *, integer const P_T *),
             double const P_T *par, integer const P_T *npar1,
             integer const P_T *npar2, double const P_T *epsiln)
{
    /* System generated locals */
    double d__1;

    /* Local variables */
    integer i;
    double f0, f1, fm, xm, test;

    *xzero = *x0;
    for (i = 1; i <= 2000; ++i) {
        f0 = (*funct)(x0, par, npar1, npar2);
        f1 = (*funct)(x1, par, npar1, npar2);
        if (f0 == 0.) {
            *xzero = *x0;
            goto L20;
        } else if (f1 == 0.) {
            *xzero = *x1;
            goto L20;
        }
        xm = (*x0 + *x1) * .5;
        if ((d__1 = *x0 - *x1, abs(d__1)) >= *epsiln) {
            fm = (*funct)(&xm, par, npar1, npar2);
            test = f0 * fm;
            if (test < 0.) {
                *x1 = xm;
            } else {
                *x0 = xm;
            }
        } else {
            *xzero = xm;
            goto L20;
        }
    }
L20:
    return;
} /* auxzfn_ */

