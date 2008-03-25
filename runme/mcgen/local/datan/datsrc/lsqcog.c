#include <stdlib.h>
#include "datsrc.h"
#include "grsrc.h"

/* Common Block Declarations */

/*
struct {
    double ysav[1000];
} dasv04_;

#define dasv04_1 dasv04_
*/

void LIBFUNC lsqcog_(integer const P_T *ix, integer const P_T *jx,
             double const P_T *xi0, double const P_T *xj0,
             double const P_T *dxi, double const P_T *dxj,
             integer const P_T *nxi, integer const P_T *nxj,
             double const P_T *fcont, double P_T *y, double P_T *cy,
             double P_T *gy, double P_T *f, double P_T *e,
             integer const P_T *m, integer const P_T *n,
             integer const P_T *nr, double P_T *x0, double P_T *a2)
{
    /* System generated locals */
    integer cy_dim1, cy_offset, e_dim1, e_offset, gy_dim1, gy_offset, f_dim1,
            f_offset, a2_dim1, a2_offset, i__1, i__2;

    /* Local variables */
    integer *list, is, nred, i, j, nstep;
    /*integer list[100];*/
    double *ysav, cx, fun[4] /* was [2][2] */, r, xi[2], xj, xjo;
    unsigned long amem;

    if ((amem = sizeof(double) * *n + sizeof(integer) * *nr) > MAXALLOC) {
        MEMERROR("lsqcog_: (too large)");
        goto f_0;
    }
    ysav = (double *)MEMALLOC(amem);
    if (!ysav) {
        MEMERROR("lsqcog_");
        goto f_0;
    }
    list = (integer *)(ysav + *n);

    for (i = 0; i < *nr; ++i)
        *(list + i) = 0;

    /* Parameter adjustments */
    a2_dim1 = *n;
    a2_offset = a2_dim1 + 1;
    a2 -= a2_offset;
    e_dim1 = *m;
    e_offset = e_dim1 + 1;
    e -= e_offset;
    f_dim1 = *n;
    f_offset = f_dim1 + 1;
    f -= f_offset;
    gy_dim1 = *n;
    gy_offset = gy_dim1 + 1;
    gy -= gy_offset;
    cy_dim1 = *n;
    cy_offset = cy_dim1 + 1;
    cy -= cy_offset;
    --y;
    --x0;

    /* Function Body */
    mtxcpv_(&y[1], ysav, n);
    i__1 = *nxi - 1;
    for (i = 0; i <= i__1; ++i) {
        xi[0] = *xi0 + i * *dxi;
        xi[1] = xi[0] + *dxi;
        i__2 = *nxj;
        for (j = 0; j <= i__2; ++j) {
            xj = *xj0 + j * *dxj;
            for (is = 1; is <= 2; ++is) {
		mtxcpv_(ysav, &y[1], n);
		x0[*ix] = xi[is - 1];
		x0[*jx] = xj;
		nstep = -100;
		nred = 0;
		lsqgen_(&y[1], &cy[cy_offset], &gy[gy_offset],
			&f[f_offset], &e[e_offset], m, n, nr, &nred, list,
			&x0[1], &cx, &r, &a2[a2_offset], &nstep);
		fun[is + 1] = r;
            }
            if (j > 0)
                grpxct_(xi, &xi[1], &xjo, &xj, fun, &fun[2],
                       &fun[1], &fun[3], fcont);
            xjo = xj;
            fun[0] = fun[2];
            fun[1] = fun[3];
        }
    }
    mtxcpv_(ysav, &y[1], n);
    MEMFREE((void *)ysav);
f_0:
    return;
} /* lsqcog_ */

