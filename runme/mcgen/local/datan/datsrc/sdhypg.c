#include <math.h>
#include "datsrc.h"

double LIBFUNC sdhypg_(integer const P_T *k, integer const P_T *n,
                      integer const P_T *kk, integer const P_T *nn)
{
    /* System generated locals */
    double ret_val, d__1, d__2, d__3;

    /* Local variables */
    double a, b, c, ak, al, an, akk, all, ann;

    an = (double) (*n);
    ann = (double) (*nn);
    ak = (double) (*k);
    akk = (double) (*kk);
    al = an - ak;
    all = ann - akk;
    if (ak <= akk && an <= ann && al <= all) {
        d__1 = akk + 1.;
        d__2 = ak + 1.;
        d__3 = akk - ak + 1.;
        a = glngam_(&d__1) - glngam_(&d__2) - glngam_(&d__3);
        d__1 = ann + 1.;
        d__2 = an + 1.;
        d__3 = ann - an + 1.;
        b = glngam_(&d__1) - glngam_(&d__2) - glngam_(&d__3);
        d__1 = all + 1.;
        d__2 = al + 1.;
        d__3 = all - al + 1.;
        c = glngam_(&d__1) - glngam_(&d__2) - glngam_(&d__3);
        ret_val = exp(a + c - b);
    } else {
        ret_val = 0.;
    }
    return ret_val;
} /* sdhypg_ */

