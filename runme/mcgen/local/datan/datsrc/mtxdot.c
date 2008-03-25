#include "datsrc.h"

void LIBFUNC mtxdot_(double const P_T *F_P(u), double const P_T *F_P(v),
                    double P_T *s, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i;

#if defined(USEHUGE)
    HFP(double,u); HFP(double,v); 
#endif

    /* Parameter adjustments */
    --v;
    --u;

    /* Function Body */
    *s = 0.f;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        *s += u[i] * v[i];
    }
    return;
} /* mtxdot_ */

