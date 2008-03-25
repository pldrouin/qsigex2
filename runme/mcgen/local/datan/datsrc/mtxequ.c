#include "datsrc.h"

void LIBFUNC mtxequ_(double P_T *F_P(a), double P_T *F_P(b),
                    integer const P_T *n, integer const P_T *m)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3;
    double d__1;

    /* Local variables */
    double amax, save;
    integer i, j, k, l, i1, kk;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); 
#endif

    /* Parameter adjustments */
    a_dim1 = *n;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *n;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    i__1 = *n - 1;
    for (k = 1; k <= i__1; ++k) {
        amax = 0.f;
        kk = k;
        i__2 = *n;
        for (l = k; l <= i__2; ++l) {
            if (abs(amax) < (d__1 = a[l + k * a_dim1], abs(d__1))) {
                amax = a[l + k * a_dim1];
                kk = l;
            }
        }
        if (kk != k) {
            i__2 = *n;
            for (j = k; j <= i__2; ++j) {
                save = a[k + j * a_dim1];
                a[k + j * a_dim1] = a[kk + j * a_dim1];
                a[kk + j * a_dim1] = save;
            }
            i__2 = *m;
            for (i = 1; i <= i__2; ++i) {
                save = b[k + i * b_dim1];
                b[k + i * b_dim1] = b[kk + i * b_dim1];
                b[kk + i * b_dim1] = save;
            }
        }
        i__2 = *n;
        for (i = k + 1; i <= i__2; ++i) {
            i__3 = *n;
            for (j = k + 1; j <= i__3; ++j) {
                a[i + j * a_dim1] -= a[k + j * a_dim1] * a[i + k * a_dim1] / 
                        a[k + k * a_dim1];
            }
            i__3 = *m;
            for (j = 1; j <= i__3; ++j) {
                b[i + j * b_dim1] -= b[k + j * b_dim1] * a[i + k * a_dim1] / 
                        a[k + k * a_dim1];
            }
        }
    }
    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
        b[*n + j * b_dim1] /= a[*n + *n * a_dim1];
        if (*n > 1) {
            i__2 = *n - 1;
            for (i1 = 1; i1 <= i__2; ++i1) {
                i = *n - i1;
                i__3 = *n;
                for (l = i + 1; l <= i__3; ++l) {
                    b[i + j * b_dim1] -= a[i + l * a_dim1] * b[l + j * b_dim1];
                }
                b[i + j * b_dim1] /= a[i + i * a_dim1];
            }
        }
    }
    return;
} /* mtxequ_ */

