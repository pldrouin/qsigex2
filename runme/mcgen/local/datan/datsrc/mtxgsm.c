#include "datsrc.h"

void LIBFUNC mtxgsm_(double const P_T *F_P(a), double P_T *F_P(s),
                    integer const P_T *m, integer const P_T *n,
                    integer const P_T *k, integer const P_T *l,
                    integer const P_T *m1, integer const P_T *n1)
{
    /* System generated locals */
    integer a_dim1, a_offset, s_dim1, s_offset, i__1, i__2;

    /* Local variables */
    integer i, j;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,s);
#endif

    /* Parameter adjustments */
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    s_dim1 = *k;
    s_offset = s_dim1 + 1;
    s -= s_offset;

    /* Function Body */
    i__1 = *k;
    for (i = 1; i <= i__1; ++i) {
        i__2 = *l;
        for (j = 1; j <= i__2; ++j) {
            s[i + j * s_dim1] = a[*m1 - 1 + i + (*n1 - 1 + j) * a_dim1];
        }
    }
    return;
} /* mtxgsm_ */

