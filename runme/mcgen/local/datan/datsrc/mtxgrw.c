#include "datsrc.h"

void LIBFUNC mtxgrw_(double const P_T *F_P(a), double P_T *F_P(r),
                    integer const P_T *m, integer const P_T *n,
                    integer const P_T *i)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;

    /* Local variables */
    integer j;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,r);
#endif

    /* Parameter adjustments */
    --r;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        r[j] = a[*i + j * a_dim1];
    }
    return;
} /* mtxgrw_ */

