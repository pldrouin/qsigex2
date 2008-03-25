#include "datsrc.h"

void LIBFUNC mtxchi_(double P_T *F_P(a), double P_T *F_P(u),
                   integer const P_T *n)
{
    /* System generated locals */
    integer a_dim1, a_offset, u_dim1, u_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i, k, l;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,u);
#endif

/* Step 1: Cholesky decomposition */
    /* Parameter adjustments */
    u_dim1 = *n;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    mtxchl_((double P_T *)&a[a_offset], (double P_T *)&u[u_offset], n);
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* Step 2: Forward Substitution */
        i__2 = *n;
        for (l = i; l <= i__2; ++l) {
            if (l == i) {
                a[*n + l * a_dim1] = 1. / u[l + l * u_dim1];
            } else {
                a[*n + l * a_dim1] = 0.;
                i__3 = l - 1;
                for (k = i; k <= i__3; ++k) {
                    a[*n + l * a_dim1] -= u[k + l * u_dim1] * a[*n + k *a_dim1];
                }
                a[*n + l * a_dim1] /= u[l + l * u_dim1];
            }
        }
/* Step 3: Back Substitution */
        i__2 = i;
        for (l = *n; l >= i__2; --l) {
            if (l == *n) {
                a[i + l * a_dim1] = a[*n + l * a_dim1] / u[l + l * u_dim1];
            } else {
                a[i + l * a_dim1] = a[*n + l * a_dim1];
                i__3 = l + 1;
                for (k = *n; k >= i__3; --k) {
                    a[i + l * a_dim1] -= u[l + k * u_dim1] * a[i + k * a_dim1]
                            ;
                }
                a[i + l * a_dim1] /= u[l + l * u_dim1];
            }
        }
    }
/* Fill lower triangle symmetrically */
    if (*n > 1) {
        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
            i__2 = i - 1;
            for (l = 1; l <= i__2; ++l) {
                a[i + l * a_dim1] = a[l + i * a_dim1];
            }
        }
    }
    return;
} /* mtxchi_ */

