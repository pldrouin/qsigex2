#include "datsrc.h"

void LIBFUNC mtxgcl_(double const P_T *F_P(a), double P_T *F_P(c),
                    integer const P_T *m, integer const P_T *n,
                    integer const P_T *i)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1;

    /* Local variables */
    integer j;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,c); 
#endif

    /* Parameter adjustments */
    --c;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
        c[j] = a[j + *i * a_dim1];
    }
    return;
} /* mtxgcl_ */

