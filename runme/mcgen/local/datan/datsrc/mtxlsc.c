#include <stdlib.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double up[1050], bb[1050], p2[1000], s[1000], v[1000];
} dasv02_;

#define dasv02_1 dasv02_
*/

void LIBFUNC mtxlsc_(double P_T *F_P(a), double P_T *b, double P_T *F_P(e),
             double const P_T *d, double P_T *x, double P_T *r,
             double P_T *F_P(a2), integer const P_T *m, integer const P_T *n,
             integer const P_T *l, double const P_T *frac, logical P_T *ok)
{
    /* Table of constant values */

    integer c__1 = 1;

    /* System generated locals */
    integer a_dim1, a_offset, e_dim1, e_offset, a2_dim1, a2_offset, i__1,
            i__2, i__3;

    /* Local variables */
    integer i, j, k, nminl, l2;
    double *up, *bb, *p2, *s, *v;
    unsigned long amem;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,e); HFP(double,a2); 
#endif

    if ((amem = sizeof(double) * (2 * *l + *n)) > MAXALLOC) {
        MEMERROR("mtxlsc_: up (too large)");
        goto f_0;
    }
    up = (double *)MEMALLOC(amem);
    if (!up) {
        MEMERROR("mtxlsc_: up");
        goto f_0;
    }
    if ((amem = sizeof(double) * 2 * *n) > MAXALLOC) {
        MEMERROR("mtxlsc_: s (too large)");
        goto f_1;
    }
    s = (double *)MEMALLOC(amem);
    if (!s) {
        MEMERROR("mtxlsc_: s");
        goto f_1;
    }
    bb = up + *l; p2 = bb + *l; v = s + *n;

/* step 1 */
    /* Parameter adjustments */
    a2_dim1 = *m;
    a2_offset = a2_dim1 + 1;
    a2 -= a2_offset;
    --b;
    --x;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --d;
    e_dim1 = *l;
    e_offset = e_dim1 + 1;
    e -= e_offset;

    /* Function Body */
    nminl = *n - *l;
    i__1 = *l;
    for (i = 1; i <= i__1; ++i) {
        mtxgrw_((double P_T *)&e[e_offset], v, l, n, &i);
        i__2 = i + 1;
        mtxhsd_(v, &up[i - 1], &bb[i - 1], n, &i, &i__2);
        i__2 = *l;
        for (j = i; j <= i__2; ++j) {
            mtxgrw_((double P_T *)&e[e_offset], s, l, n, &j);
            i__3 = i + 1;
            mtxhst_(v, &up[i - 1], &bb[i - 1], s, n, &i, &i__3);
            if (j == i && *n > i) {
                i__3 = *n;
                for (k = i + 1; k <= i__3; ++k) {
                    s[k - 1] = v[k - 1];
                }
            }
            mtxprw_((double P_T *)&e[e_offset], s, l, n, &j);
        }
        i__2 = *m;
        for (j = 1; j <= i__2; ++j) {
            mtxgrw_((double P_T *)&a[a_offset], s, m, n, &j);
            i__3 = i + 1;
            mtxhst_(v, &up[i - 1], &bb[i - 1], s, n, &i, &i__3);
            mtxprw_((double P_T *)&a[a_offset], s, m, n, &j);
        }
    }
/* step 2 */
    x[1] = d[1] / e[e_dim1 + 1];
    if (*l > 1) {
        i__1 = *l;
        for (j = 2; j <= i__1; ++j) {
            x[j] = d[j];
            i__2 = j - 1;
            for (k = 1; k <= i__2; ++k) {
                x[j] -= e[j + k * e_dim1] * x[k];
            }
            x[j] /= e[j + j * e_dim1];
        }
    }
/* step 3 */
    i__1 = *m;
    for (j = 1; j <= i__1; ++j) {
        i__2 = *l;
        for (k = 1; k <= i__2; ++k) {
            b[j] -= a[j + k * a_dim1] * x[k];
        }
    }
/* step 4 */
    l2 = 1;
    i__1 = *l + 1;
    mtxgsm_((double P_T *)&a[a_offset], (double P_T *)&a2[a2_offset],
           m, n, m, &nminl, &c__1, &i__1);
    mtxsvd_((double P_T *)&a2[a2_offset], &b[1], p2,
           r, m, &nminl, &l2, frac, ok);
    if (*ok) {
        i__1 = *l + 1;
        mtxpsm_(&x[1], p2, n, &c__1, &nminl, &c__1, &i__1, &c__1);
        for (i = *l; i >= 1; --i) {
            mtxgrw_((double P_T *)&e[e_offset], v, l, n, &i);
            i__1 = i + 1;
            mtxhst_(v, &up[i - 1], &bb[i - 1], &x[1], n, &i, &i__1);
        }
    }
    MEMFREE((void *)s);
f_1:
    MEMFREE((void *)up);
f_0:
    return;
} /* mtxlsc_ */

