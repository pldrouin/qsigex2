#include <stdlib.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double e[1000];
} dasv04_;

#define dasv04_1 dasv04_
*/

void LIBFUNC mtxdec_(double P_T *F_P(a), double P_T *F_P(b), double P_T *F_P(x),
             double P_T *r, integer const P_T *m, integer P_T *n,
             double const P_T *frac, logical P_T *ok, double P_T *d,
             double P_T *u, double P_T *v)
{
    /* Table of constant values */

    integer c__1 = 1;

    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, x_dim1, x_offset, u_dim1,
            u_offset, v_dim1, v_offset;

    /* Local variables */
    double *e;
    unsigned long amem;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,x);
#endif

    if ((amem = sizeof(double) * *n) > MAXALLOC) {
        MEMERROR("mtxdec_ (too large)");
        goto f_0;
    }
    e = (double *)MEMALLOC(amem);
    if (!e) {
        MEMERROR("mtxdec_");
        goto f_0;
    }

/* STEP 0: Set B equal to (N x N) unit matrix */
    /* Parameter adjustments */
    u_dim1 = *m;
    u_offset = u_dim1 + 1;
    u -= u_offset;
    --r;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    v_dim1 = *n;
    v_offset = v_dim1 + 1;
    v -= v_offset;
    --d;
    x_dim1 = *n;
    x_offset = x_dim1 + 1;
    x -= x_offset;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    mtxunt_(&b[b_offset], m);
/* STEP 1: Bidiagonalisation of A */
    mtxsv1_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset], &d[1],
           e, m, n, m);
/* STEP 2: Diagonalisation of bidiagonal matrix */
    mtxsv2_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset], &d[1],
           e, m, n, m, ok);
/* STEP 3: Order singular values and perform permutations */
    mtxsv3_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset], &d[1],
           m, n, m);
    mtxgsm_((double P_T *)&a[a_offset], &v[v_offset], m, n, n, n, &c__1, &c__1);
    mtxtrp_((double P_T *)&b[b_offset], &u[u_offset], m, m);
/* STEP 4: Singular value analysis */
    mtxsv4_((double P_T *)&a[a_offset], (double P_T *)&b[b_offset], &d[1],
           (double P_T *)&x[x_offset], &r[1], m, n, m, frac);
    MEMFREE((void *)e);
f_0:
    return;
} /* mtxdec_ */

