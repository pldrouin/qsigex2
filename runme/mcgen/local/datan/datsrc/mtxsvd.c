#include <stdlib.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double d[1000], e[1000];
} dasv01_;

#define dasv01_1 dasv01_
*/

void LIBFUNC mtxsvd_(double P_T *F_P(a), double P_T *F_P(b), double P_T *F_P(x),
             double P_T *F_P(r), integer const P_T *m, integer P_T *n,
             integer const P_T *nb, double const P_T *frac, logical P_T *ok)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset;

    /* Local variables */
    double *d, *e;
    unsigned long amem;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,x); HFP(double,r); 
#endif

    if ((amem = sizeof(double) * 2 * *n) > MAXALLOC) {
        MEMERROR("mtxsvd_ (too large)");
        goto f_0;
    }
    d = (double *)MEMALLOC(amem);
    if (!d) {
        MEMERROR("mtxsvd_");
        goto f_0;
    }
    e = d + *n;

/* STEP 1: Bidiagonalisation of A */
    /* Parameter adjustments */
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --r;
    x_dim1 = *n;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    mtxsv1_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset],
           d, e, m, n, nb);
/* STEP 2: Diagonalisation of bidiagonal matrix */
    mtxsv2_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset],
           d, e, m, n, nb, ok);
/* STEP 3: Order singular values and perform  permutations */
    mtxsv3_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset],
           d, m, n, nb);
/* STEP 4: Singular value analysis */
    mtxsv4_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset], d,
           (double P_T *)&x[x_offset], (double P_T *)&r[1], m, n, nb, frac);
    MEMFREE((void *)d);
f_0:
    return;
} /* mtxsvd_ */

