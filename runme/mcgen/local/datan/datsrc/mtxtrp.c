#include "datsrc.h"

void LIBFUNC mtxtrp_(double const P_T *F_P(a), double P_T *F_P(r),
                    integer const P_T *m, integer const P_T *n)
{
    /* System generated locals */
    integer a_dim1, a_offset, r_dim1, r_offset, i__1, i__2;

    /* Local variables */
    integer i, j;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,r); 
#endif

    /* Parameter adjustments */
    r_dim1 = *n;
    r_offset = r_dim1 + 1;
    r -= r_offset;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *m;
        for (i = 1; i <= i__2; ++i) {
            r[j + i * r_dim1] = a[i + j * a_dim1];
        }
    }
    return;
} /* mtxtrp_ */

