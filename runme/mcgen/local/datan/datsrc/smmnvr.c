#include <math.h>
#include "datsrc.h"

void LIBFUNC smmnvr_(double const P_T *data, integer const P_T *n,
             double P_T *xmean, double P_T *delxm, double P_T *s2,
             double P_T *dels2, double P_T *s, double P_T *dels)
{
    /* System generated locals */
    integer i__1;
    double d__1;

    /* Local variables */
    integer i;
    double q, xn, xn1;

    /* Parameter adjustments */
    --data;

    /* Function Body */
    *xmean = 0.;
    xn = (double) (*n);
    xn1 = xn - 1.;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        *xmean += data[i];
    }
    *xmean /= xn;
    if (*n > 1) {
        q = 0.;
        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
/* Computing 2nd power */
            d__1 = data[i] - *xmean;
            q += d__1 * d__1;
        }
        *s2 = q / xn1;
        *s = sqrt(*s2);
        *delxm = *s / sqrt(xn);
        *dels2 = *s2 * sqrt(2. / xn1);
        *dels = *s / sqrt(xn1 * 2.);
    } else {
        *delxm = 0.;
        *s2 = 0.;
        *s = 0.;
        *dels2 = 0.;
        *dels = 0.;
    }
    return;
} /* smmnvr_ */

