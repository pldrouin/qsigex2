#include "datsrc.h"

void LIBFUNC mtxsv3_(double P_T *F_P(a), double P_T *F_P(b), double P_T *F_P(d),
             integer const P_T *m, integer const P_T *n, integer const P_T *nb)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2;

    /* Local variables */
    integer i, j, k;
    double t;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,d); 
#endif

/* Order singular values */
    /* Parameter adjustments */
    --d;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    if (*n > 1) {
L10:
        i__1 = *n;
        for (i = 2; i <= i__1; ++i) {
            if (d[i] > d[i - 1]) {
                goto L30;
            }
        }
        return;
L30:
        i__1 = *n;
        for (i = 2; i <= i__1; ++i) {
            t = d[i - 1];
            k = i - 1;
            i__2 = *n;
            for (j = i; j <= i__2; ++j) {
                if (t < d[j]) {
                    t = d[j];
                    k = j;
                }
            }
            if (k != i - 1) {
/* perform permutation on singular values */
                d[k] = d[i - 1];
                d[i - 1] = t;
/* perform permutation on matrix A */
                i__2 = *n;
                for (j = 1; j <= i__2; ++j) {
                    t = a[j + k * a_dim1];
                    a[j + k * a_dim1] = a[j + (i - 1) * a_dim1];
                    a[j + (i - 1) * a_dim1] = t;
                }
/* perform permutation on matrix B */
                i__2 = *nb;
                for (j = 1; j <= i__2; ++j) {
                    t = b[k + j * b_dim1];
                    b[k + j * b_dim1] = b[i - 1 + j * b_dim1];
                    b[i - 1 + j * b_dim1] = t;
                }
            }
        }
        goto L10;
    }
    return;
} /* mtxsv3_ */

