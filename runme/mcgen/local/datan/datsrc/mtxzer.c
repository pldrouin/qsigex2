#include "datsrc.h"

void LIBFUNC mtxzer_(double P_T *F_P(r), integer const P_T *n,
                    integer const P_T *m)
{
    /* System generated locals */
    integer r_dim1, r_offset, i__1, i__2;

    /* Local variables */
    integer i, j;

#if defined(USEHUGE)
    HFP(double,r); 
#endif

    /* Parameter adjustments */
    r_dim1 = *n;
    r_offset = r_dim1 + 1;
    r -= r_offset;

    /* Function Body */
    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            r[i + j * r_dim1] = 0.f;
        }
    }
    return;
} /* mtxzer_ */

