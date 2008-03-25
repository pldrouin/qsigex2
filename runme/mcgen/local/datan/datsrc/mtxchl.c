#include <math.h>
#include "datsrc.h"

void LIBFUNC mtxchl_(double const P_T *F_P(a), double P_T *F_P(u),
             integer const P_T *n)
{
    /* System generated locals */
    integer a_dim1, a_offset, u_dim1, u_offset, i__1, i__2, i__3;
    double d__1;

    /* Local variables */
    integer j, k, l;
    double s;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,u);
#endif

    /* Parameter adjustments */
    u_dim1 = *n;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    mtxzer_((double P_T *)&u[u_offset], n, n);
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
        s = 0.f;
        i__2 = *n;
        for (j = k; j <= i__2; ++j) {
            if (k > 1) {
                s = 0.f;
                i__3 = k - 1;
                for (l = 1; l <= i__3; ++l) {
                    s += u[l + k * u_dim1] * u[l + j * u_dim1];
                }
            }
            u[k + j * u_dim1] = a[k + j * a_dim1] - s;
            if (k == j) {
                u[k + j * u_dim1] = sqrt((d__1 = u[k + j * u_dim1], abs(d__1)));
            } else {
                u[k + j * u_dim1] /= u[k + k * u_dim1];
            }
        }
    }
    return;
} /* mtxchl_ */

