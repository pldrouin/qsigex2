#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double d[1050], t[1050], b[1050], u[1050];
} dasv03_;

#define dasv03_1 dasv03_
*/

void LIBFUNC lsqgen_(double P_T *y, double P_T *cy, double P_T *fy,
             double P_T *f, double P_T *e, integer const P_T *m,
             integer const P_T *n, integer const P_T *nr,
             integer const P_T *nred, integer P_T *list, double P_T *x,
             double P_T *cx, double P_T *r, double P_T *a2, integer P_T *nstep)
{
    /* Table of constant values */

    integer c__1 = 1;
    double c_dm1 = -1., c_d0 = 0.;

    /* System generated locals */
    integer cy_dim1, cy_offset, cx_dim1, cx_offset, e_dim1, e_offset, fy_dim1,
             fy_offset, f_dim1, f_offset, a2_dim1, a2_offset, i__1, i__2,
            i__3;
    double d__1;

    /* Local variables */
    integer ired, i, k, l, istep;
    double rlst;
    logical ok;
    logical covmat;
    double *b, *d, *u, *t;
    unsigned long amem;

    if ((amem = sizeof(double) * 2 * (*n + *nred)) > MAXALLOC) {
        MEMERROR("lsqgen_: b (too large)");
        goto f_0;
    }
    b = (double *)MEMALLOC(amem);
    if (!b) {
        MEMERROR("lsqgen_: b");
        goto f_0;
    }
    if ((amem = sizeof(double) * (*m + max(*nr,*n + *nred))) > MAXALLOC) {
        MEMERROR("lsqgen_: d (too large)");
        goto f_1;
    }
    d = (double *)MEMALLOC(amem);
    if (!d) {
        MEMERROR("lsqgen_: d");
        goto f_1;
    }
    t = b + *n + *nred;
    u = d + *m;

/* general case of least squares fitting */
    /* Parameter adjustments */
    fy_dim1 = *n;
    fy_offset = fy_dim1 + 1;
    fy -= fy_offset;
    cy_dim1 = *n;
    cy_offset = cy_dim1 + 1;
    cy -= cy_offset;
    --y;
    --x;
    --list;
    a2_dim1 = *n + *nred;
    a2_offset = a2_dim1 + 1;
    a2 -= a2_offset;
    cx_dim1 = *nred;
    cx_offset = cx_dim1 + 1;
    cx -= cx_offset;
    e_dim1 = *m;
    e_offset = e_dim1 + 1;
    e -= e_offset;
    f_dim1 = *n + *nred;
    f_offset = f_dim1 + 1;
    f -= f_offset;

    /* Function Body */
    ok = TRUE_;
    covmat = TRUE_;
    if (*nstep < 0) {
        covmat = FALSE_;
        *nstep = abs(*nstep);
    }
    if (*nstep < 1) {
        *nstep = 100;
    }
    if (*nr > 0) {
/* For NR=NRED :  set LIST */
        if (*nr == *nred) {
            i__1 = *nr;
            for (i = 1; i <= i__1; ++i) {
                list[i] = 1;
            }
        }
    }
    l = *n + *nred;
    mtxzrv_(t, &l);
    mtxtra_(&cy[cy_offset], &f[f_offset], n, n);
    mtxchi_(&cy[cy_offset], &fy[fy_offset], n);
    mtxchl_(&cy[cy_offset], &fy[fy_offset], n);
    mtxtra_(&f[f_offset], &cy[cy_offset], n, n);
/* start iteration */
    *r = 0.;
    i__1 = *nstep;
    for (istep = 1; istep <= i__1; ++istep) {
        rlst = *r;
        mtxzer_(&f[f_offset], &l, &l);
        i__2 = *nred + 1;
        i__3 = *nred + 1;
        mtxpsm_(&f[f_offset], &fy[fy_offset], &l, &l, n, n, &i__2, &i__3);
        i__2 = *m;
        for (k = 1; k <= i__2; ++k) {
            d[k - 1] = -lsqgfn_(&y[1], &x[1], n, nr, &k);
        }
/* Numerical Derivatives */
        auxdrg_(&x[1], &y[1], m, n, nr, nred, &list[1], &e[e_offset], &ok);
        if (! ok) {
            *nstep = -3;
            goto L60;
        }
        mtxchm_(&f[f_offset], t, b, &l, &c__1);
        mtxmsv_(b, b, &c_dm1, &l);
        mtxlsc_(&f[f_offset], b, &e[e_offset], d,
                u, r, &a2[a2_offset], &l, &l, m, &c_d0, &ok);
        if (! ok) {
            *nstep = -1;
            goto L60;
        }
        if (*nred > 0) {
            ired = 0;
            i__2 = *nr;
            for (i = 1; i <= i__2; ++i) {
                if (list[i] != 0) {
                    ++ired;
                    x[i] += u[ired - 1];
                }
            }
        }
        i__2 = *n;
        for (i = 1; i <= i__2; ++i) {
            y[i] += u[i + *nred - 1];
            t[i + *nred - 1] += u[i + *nred - 1];
        }
/* test for convergence */
        if (istep > 1 && (d__1 = *r - rlst, abs(d__1)) < *r * 1e-8 + 1e-15) {
            *nstep = istep;
            if (covmat) {
/* compute matrix GB */
                auxdrg_(&x[1], &y[1], m, n, nr, nred, &list[1], &e[e_offset],
                        &ok);
                if (!ok) {
                    *nstep = -3;
                    goto L60;
                }
                i__2 = *nred + 1;
                mtxgsm_(&e[e_offset], &a2[a2_offset], m, &l, m, n, &c__1, &i__2);
                mtxmbt_(&cy[cy_offset], &a2[a2_offset], &f[f_offset], n, n, m);
                mtxmlt_(&a2[a2_offset], &f[f_offset], &fy[fy_offset], m, n, m);
                mtxchi_(&fy[fy_offset], &f[f_offset], m);
/* array FY now contains matrix GB */
                if (*nred > 0) {
                    mtxgsm_(&e[e_offset], &a2[a2_offset], m, &l, m, nred,
                           &c__1, &c__1);
                    mtxmat_(&a2[a2_offset], &fy[fy_offset], &f[f_offset],
                            nred, m, m);
                    mtxmlt_(&f[f_offset], &a2[a2_offset], &cx[cx_offset],
                            nred, m, nr);
                    mtxchi_(&cx[cx_offset], &f[f_offset], nred);
/* array CX now contains covariance matrix of unknowns */
                } else {
                    mtxmlt_(&a2[a2_offset], &cy[cy_offset], &f[f_offset], m,
                            n, n);
                    mtxmlt_(&fy[fy_offset], &f[f_offset], &a2[a2_offset], m,
                            m, n);
                    mtxmat_(&f[f_offset], &a2[a2_offset], &fy[fy_offset], n,
                            m, n);
                    mtxsub_(&cy[cy_offset], &fy[fy_offset], &cy[cy_offset],
                            n, n);
/* array CY now contains covariance matrix of 'improved' */
/* measurements */
                }
            }
            goto L60;
        }
    }
    *nstep = -2;
L60:
    MEMFREE((void *)d);
f_1:
    MEMFREE((void *)b);
f_0:
    return;
} /* lsqgen_ */

