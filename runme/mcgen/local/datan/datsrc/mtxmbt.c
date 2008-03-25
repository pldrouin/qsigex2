#include "datsrc.h"

void LIBFUNC mtxmbt_(double const P_T *F_P(a), double const P_T *F_P(b),
                    double P_T *F_P(r), integer const P_T *m,
                    integer const P_T *l, integer const P_T *n)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, r_dim1, r_offset, i__1, i__2, 
            i__3;

    /* Local variables */
    integer i, j, ll;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,r);
#endif

    /* Parameter adjustments */
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    r_dim1 = *m;
    r_offset = r_dim1 + 1;
    r -= r_offset;
    b_dim1 = *n;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *m;
        for (i = 1; i <= i__2; ++i) {
            r[i + j * r_dim1] = 0.f;
            i__3 = *l;
            for (ll = 1; ll <= i__3; ++ll) {
                r[i + j * r_dim1] += a[i + ll * a_dim1] * b[j + ll * b_dim1];
            }
        }
    }
    return;
} /* mtxmbt_ */

