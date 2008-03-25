#include "datsrc.h"

void LIBFUNC minprb_(double P_T *xmp, double const P_T *xa,
             double const P_T *ya, double const P_T *xb,
             double const P_T *yb, double const P_T *xc,
             double const P_T *yc)
{
    /* System generated locals */
    double d__1, d__2;

    /* Local variables */
    double anum, f1, f2, den, xba, xbc;

    xba = *xb - *xa;
    xbc = *xb - *xc;
    f1 = xba * (*yb - *yc);
    f2 = xbc * (*yb - *ya);
    anum = xba * f1 - xbc * f2;
    den = f1 - f2;
/* Computing MAX */
    d__2 = abs(den);
    d__1 = max(d__2,1e-10);
    den = den >= 0. ? d__1 : -d__1;
    /*den = d_sign(&d__1, &den);*/
    *xmp = *xb - anum / (den * 2.);
    return;
} /* minprb_ */

