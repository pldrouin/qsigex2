#include "datsrc.h"

void LIBFUNC mtxpsv_(double P_T *F_P(u), double const P_T *F_P(v),
                    integer const P_T *n, integer const P_T *nred,
                    integer const P_T *F_P(list))
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer ired, i;

#if defined(USEHUGE)
    HFP(double,u); HFP(double,v); HFP(integer,list);
#endif

    /* Parameter adjustments */
    --list;
    --u;
    --v;

    /* Function Body */
    ired = 0;
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        if (list[i] == 1) {
            ++ired;
            u[i] = v[ired];
        }
    }
    return;
} /* mtxpsv_ */

