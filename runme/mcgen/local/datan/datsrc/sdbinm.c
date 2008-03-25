#include <math.h>
#include "datsrc.h"

double LIBFUNC sdbinm_(integer const P_T *k, integer const P_T *n,
                      double const P_T *p)
{
    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

    /* Local variables */
    double ak, an, albinc, alfact, absarg, arg;

    ak = (double) (*k);
    an = (double) (*n);
    if (*n < 1 || *k < 0 || *k > *n) {
        ret_val = 0.;
    } else {
        d__1 = an + 1.;
        d__2 = ak + 1.;
        d__3 = an - ak + 1.;
        albinc = glngam_(&d__1) - glngam_(&d__2) - glngam_(&d__3);
        if (*p <= 0.) {
            if (*k == 0) {
                ret_val = 1.;
            } else {
                ret_val = 0.;
            }
        } else if (*p >= 1.) {
            if (*k == *n) {
                ret_val = 1.;
            } else {
                ret_val = 0.;
            }
        } else {
            alfact = log(*p) * ak + log(1. - *p) * (an - ak);
            arg = alfact + albinc;
            absarg = abs(arg);
            if (absarg < 1e-4) {
                ret_val = 1.;
            } else if (absarg > 100.) {
                ret_val = 0.;
            } else {
                ret_val = exp(arg);
            }
        }
    }
    return ret_val;
} /* sdbinm_ */
