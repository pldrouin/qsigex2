#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

void LIBFUNC mincov_(double P_T *x0, integer const P_T *n,
             integer const P_T *nred, integer const P_T *list,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), double const P_T *fact,
             double P_T *scrat1, double P_T *scrat2,
             double P_T *cx, logical P_T *ok)
{
    /* System generated locals */
    integer cx_dim1, cx_offset, scrat1_dim1, scrat1_offset, scrat2_dim1,
            scrat2_offset;

    /* Local variables */
    integer ls, ms, ns;
    double frac, f;
    /*double r[50];*/
    double *r;
    unsigned long amem;

    if ((amem = sizeof(double) * *nred) > MAXALLOC) {
        MEMERROR("mincov_ (too large)");
        goto f_0;
    }
    r = (double *)MEMALLOC(amem);
    if (!r) {
        MEMERROR("mincov_");
        goto f_0;
    }

    /* Parameter adjustments */
    --x0;
    cx_dim1 = *nred;
    cx_offset = cx_dim1 + 1;
    cx -= cx_offset;
    scrat2_dim1 = *nred;
    scrat2_offset = scrat2_dim1 + 1;
    scrat2 -= scrat2_offset;
    scrat1_dim1 = *nred;
    scrat1_offset = scrat1_dim1 + 1;
    scrat1 -= scrat1_offset;
    --list;

    /* Function Body */
    frac = 0.;
    ms = *nred;
    ns = *nred;
    ls = *nred;
    auxhes_(&x0[1], &scrat1[scrat1_offset], n, nred, &list[1], userfn);
    mtxunt_(&scrat2[scrat2_offset], &ms);
    mtxsvd_(&scrat1[scrat1_offset], &scrat2[scrat2_offset], &cx[cx_offset], r,
             &ms, &ns, &ls, &frac, ok);
    if (*fact < 1e-10) {
        f = 1.;
    } else {
        f = *fact;
    }
    mtxmsc_(&cx[cx_offset], &cx[cx_offset], &f, nred, nred);
    MEMFREE((void *)r);
f_0:
    return;
} /* mincov_ */

