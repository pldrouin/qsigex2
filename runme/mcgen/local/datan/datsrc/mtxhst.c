#include "datsrc.h"

void LIBFUNC mtxhst_(double const P_T *F_P(v), double const P_T *up,
             double const P_T *b, double P_T *F_P(c), integer const P_T *n,
             integer const P_T *lp, integer const P_T *l)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i;
    double s, dup;

#if defined(USEHUGE)
    HFP(double,v); HFP(double,c);
#endif

    /* Parameter adjustments */
    --c;
    --v;

    /* Function Body */
    dup = *up;
    s = c[*lp] * dup;
    i__1 = *n;
    for (i = *l; i <= i__1; ++i) {
        s += c[i] * v[i];
    }
    s *= *b;
    c[*lp] += s * dup;
    i__1 = *n;
    for (i = *l; i <= i__1; ++i) {
        c[i] += s * v[i];
    }
    return;
} /* mtxhst_ */

