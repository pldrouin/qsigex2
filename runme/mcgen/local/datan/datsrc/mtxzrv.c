#include "datsrc.h"

void LIBFUNC mtxzrv_(double P_T *F_P(u), integer const P_T *n)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i;

#if defined(USEHUGE)
    HFP(double,u); 
#endif

    /* Parameter adjustments */
    --u;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        u[i] = 0.f;
    }
    return;
} /* mtxzrv_ */

