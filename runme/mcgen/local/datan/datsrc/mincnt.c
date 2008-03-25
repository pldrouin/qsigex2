#include <stdlib.h>
#include "datsrc.h"
#include "grsrc.h"

void LIBFUNC mincnt_(integer const P_T *ix, integer const P_T *jx,
             double const P_T *xi0, double const P_T *xj0,
             double const P_T *dxi, double const P_T *dxj,
             integer const P_T *nxi, integer const P_T *nxj,
             double const P_T *fcont, double P_T *x0, integer const P_T *n,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *))
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer i, j, is, js;
    double xi[2], xj[2], f[4] /* was [2][2] */;
    /*double x0sav[50];*/
    double *x0sav;
    unsigned long amem;

    if ((amem = sizeof(double) * *n) > MAXALLOC) {
        MEMERROR("mincnt_ (too large)");
        goto f_0;
    }
    x0sav = (double *)MEMALLOC(amem);
    if (!x0sav) {
        MEMERROR("mincnt_");
        goto f_0;
    }

    /* Parameter adjustments */
    --x0;

    /* Function Body */
    mtxcpv_(&x0[1], x0sav, n);
    i__1 = *nxi - 1;
    for (i = 0; i <= i__1; ++i) {
        xi[0] = *xi0 + i * *dxi;
        xi[1] = xi[0] + *dxi;
        i__2 = *nxj - 1;
        for (j = 0; j <= i__2; ++j) {
            xj[0] = *xj0 + j * *dxj;
            xj[1] = xj[0] + *dxj;
            for (is = 1; is <= 2; ++is) {
                for (js = 1; js <= 2; ++js) {
                    mtxcpv_(x0sav, &x0[1], n);
                    x0[*ix] = xi[is - 1];
                    x0[*jx] = xj[js - 1];
                    f[is + (js << 1) - 3] = (*userfn)(&x0[1], n);
                }
            }
            grpxct_(xi, &xi[1], xj, &xj[1], f, &f[2], &f[1], &f[3], fcont);
        }
    }
    mtxcpv_(x0sav, &x0[1], n);
    MEMFREE((void *)x0sav);
f_0:
    return;
} /* mincnt_ */

