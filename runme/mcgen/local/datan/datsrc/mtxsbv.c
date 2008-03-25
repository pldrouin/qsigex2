#include "datsrc.h"

void LIBFUNC mtxsbv_(double const P_T *F_P(u), double const P_T *F_P(v),
                    double P_T *F_P(w), integer const P_T *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i;

#if defined(USEHUGE)
    HFP(double,u); HFP(double,v); HFP(double,w);
#endif

    /* Parameter adjustments */
    --w;
    --v;
    --u;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        w[i] = u[i] - v[i];
    }
    return;
} /* mtxsbv_ */

