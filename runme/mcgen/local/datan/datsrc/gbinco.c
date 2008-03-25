#include <math.h>
#include "datsrc.h"

double LIBFUNC gbinco_(integer const P_T *n, integer const P_T *k)
{
    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

    /* Local variables */
    double ak, an, albinc;

    if (*k == 0 || *n == 0 || *k == *n) {
        ret_val = 1.;
    } else {
        an = (double) (*n);
        ak = (double) (*k);
        d__1 = an + 1.;
        d__2 = ak + 1.;
        d__3 = an - ak + 1.;
        albinc = glngam_(&d__1) - glngam_(&d__2) - glngam_(&d__3);
        ret_val = exp(albinc);
        ret_val = (double) nint(ret_val);
    }
    return ret_val;
} /* gbinco_ */

