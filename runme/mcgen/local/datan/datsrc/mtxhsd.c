#include <math.h>
#include "datsrc.h"

void LIBFUNC mtxhsd_(double const P_T *F_P(v), double P_T *up, double P_T *b, 
             integer const P_T *n, integer const P_T *lp, integer const P_T *l)
{
    /* System generated locals */
    integer i__1;
    double d__1, d__2;

    /* Local variables */
    integer i;
    double c, c1, sd, vpprim;

#if defined(USEHUGE)
    HFP(double,v);
#endif

    /* Parameter adjustments */
    --v;

    /* Function Body */
    c = (d__1 = v[*lp], abs(d__1));
    i__1 = *n;
    for (i = *l; i <= i__1; ++i) {
/* Computing MAX */
        d__2 = (d__1 = v[i], abs(d__1));
        c = max(d__2,c);
    }
    if (c <= 0.) {
        goto L30;
    }
    c1 = 1.f / c;
/* Computing 2nd power */
    d__1 = v[*lp] * c1;
    sd = d__1 * d__1;
    i__1 = *n;
    for (i = *l; i <= i__1; ++i) {
/* Computing 2nd power */
        d__1 = v[i] * c1;
        sd += d__1 * d__1;
    }
    vpprim = sd;
    vpprim = c * sqrt((abs(vpprim)));
    if (v[*lp] > 0.) {
        vpprim = -vpprim;
    }
    *up = v[*lp] - vpprim;
    *b = 1.f / (vpprim * *up);
L30:
    return;
} /* mtxhsd_ */

