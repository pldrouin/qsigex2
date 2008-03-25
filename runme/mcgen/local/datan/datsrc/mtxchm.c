#include "datsrc.h"

void LIBFUNC mtxchm_(double const P_T *F_P(u), double const P_T *F_P(a),
                    double P_T *F_P(r), integer const P_T *m,
                    integer const P_T *n)
{
    /* System generated locals */
    integer u_dim1, u_offset, a_dim1, a_offset, r_dim1, r_offset, i__1, i__2, 
            i__3;

    /* Local variables */
    integer i, k, l;

#if defined(USEHUGE)
    HFP(double,u); HFP(double,a); HFP(double,r);
#endif

    /* Parameter adjustments */
    u_dim1 = *m;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    r_dim1 = *m;
    r_offset = r_dim1 + 1;
    r -= r_offset;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *m;
    for (i = 1; i <= i__1; ++i) {
        i__2 = *n;
        for (k = 1; k <= i__2; ++k) {
            r[i + k * r_dim1] = 0.;
            i__3 = *m;
            for (l = i; l <= i__3; ++l) {
                r[i + k * r_dim1] += u[i + l * u_dim1] * a[l + k * a_dim1];
            }
        }
    }
    return;
} /* mtxchm_ */

