#include "datsrc.h"

void LIBFUNC mtxmsc_(double const P_T *F_P(a), double P_T *F_P(r),
                    double const P_T *s, integer const P_T *m,
                    integer const P_T *n)
{
    /* System generated locals */
    integer a_dim1, a_offset, r_dim1, r_offset, i__1, i__2;

    /* Local variables */
    integer i, j;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,r);
#endif

    /* Parameter adjustments */
    r_dim1 = *m;
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
            r[i + j * r_dim1] = *s * a[i + j * a_dim1];
        }
    }
    return;
} /* mtxmsc_ */

