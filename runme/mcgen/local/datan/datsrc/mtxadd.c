#include "datsrc.h"

void LIBFUNC mtxadd_(double const P_T *F_P(a), double const P_T *F_P(b),
                    double P_T *F_P(r), integer const P_T *m,
                    integer const P_T *n)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, r_dim1, r_offset, i__1, i__2;

    /* Local variables */
    integer i, j;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,r);
#endif

    /* Parameter adjustments */
    r_dim1 = *m;
    r_offset = r_dim1 + 1;
    r -= r_offset;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *m;
        for (i = 1; i <= i__2; ++i) {
            r[i + j * r_dim1] = a[i + j * a_dim1] + b[i + j * b_dim1];
        }
    }
    return;
} /* mtxadd_ */

