#include <stdlib.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double v[1000], s[1000], ups[1000], bbs[1000];
} dasv00_;

#define dasv00_1 dasv00_
*/

void LIBFUNC mtxsv1_(double P_T *F_P(a), double P_T *F_P(b), double P_T *F_P(d),
             double P_T *F_P(e), integer const P_T *m, integer const P_T *n,
             integer const P_T *nb)
{
    /* System generated locals */
    integer a_dim1, a_offset, b_dim1, b_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    integer i, j, k;
    double bb, up;
    double *s, *v, *bbs, *ups;
    unsigned long amem;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,b); HFP(double,d); HFP(double,e); 
#endif

    if ((amem = sizeof(double) * 2 * max(*n,*m)) > MAXALLOC) {
        MEMERROR("mtxsv1_: s (too large)");
        goto f_0;
    }
    s = (double *)MEMALLOC(amem);
    if (!s) {
        MEMERROR("mtxsv1_: s");
        goto f_0;
    }
    if ((amem = sizeof(double) * 2 * *n) > MAXALLOC) {
        MEMERROR("mtxsv1_: bbs (too large)");
        goto f_1;
    }
    bbs = (double *)MEMALLOC(amem);
    if (!bbs) {
        MEMERROR("mtxsv1_: bbs");
        goto f_1;
    }
    v = s + max(*n,*m); ups = bbs + *n;

    /* Parameter adjustments */
    --e;
    --d;
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    b_dim1 = *m;
    b_offset = b_dim1 + 1;
    b -= b_offset;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* set up Householder Transformation Q(I) */
        if (i < *n || *m > *n) {
            mtxgcl_((double P_T *)&a[a_offset], v, m, n, &i);
            i__2 = i + 1;
            mtxhsd_(v, &up, &bb, m, &i, &i__2);
/* apply Q(I) to A */
            i__2 = *n;
            for (j = i; j <= i__2; ++j) {
                mtxgcl_((double P_T *)&a[a_offset], s, m, n, &j);
                i__3 = i + 1;
                mtxhst_(v, &up, &bb, s, m, &i, &i__3);
                mtxpcl_((double P_T *)&a[a_offset], s, m, n, &j);
            }
/* apply Q(I) to B */
            i__2 = *nb;
            for (k = 1; k <= i__2; ++k) {
                mtxgcl_((double P_T *)&b[b_offset], s, m, nb, &k);
                i__3 = i + 1;
                mtxhst_(v, &up, &bb, s, m, &i, &i__3);
                mtxpcl_((double P_T *)&b[b_offset], s, m, nb, &k);
            }
        }
        if (i < *n - 1) {
/* set up Householder Transformation H(I) */
            mtxgrw_((double P_T *)&a[a_offset], v, m, n, &i);
            i__2 = i + 1;
            i__3 = i + 2;
            mtxhsd_(v, &up, &bb, n, &i__2, &i__3);
/* save H(I) */
            ups[i - 1] = up;
            bbs[i - 1] = bb;
/* apply H(I) to A */
            i__2 = *m;
            for (j = i; j <= i__2; ++j) {
                mtxgrw_((double P_T *)&a[a_offset], s, m, n, &j);
                i__3 = i + 1;
                i__4 = i + 2;
                mtxhst_(v, &up, &bb, s, n, &i__3, &i__4);
/* save elements I+2,... in row J of matrix A */
                if (j == i) {
                    i__3 = *n;
                    for (k = i + 2; k <= i__3; ++k) {
                        s[k - 1] = v[k - 1];
                    }
                }
                mtxprw_((double P_T *)&a[a_offset], s, m, n, &j);
            }
        }
    }
/* copy diagonal of transformed matrix A to D */
/* and upper parallel A to E */
    if (*n > 1) {
        i__1 = *n;
        for (i = 2; i <= i__1; ++i) {
            d[i] = a[i + i * a_dim1];
            e[i] = a[i - 1 + i * a_dim1];
        }
    }
    d[1] = a[a_dim1 + 1];
    e[1] = 0.f;
/* construct product matrix H=H(2)*H(3)*...*H(N), H(N)=I */
    for (i = *n; i >= 1; --i) {
        if (i <= *n - 1) {
            mtxgrw_((double P_T *)&a[a_offset], v, m, n, &i);
        }
        i__1 = *n;
        for (k = 1; k <= i__1; ++k) {
            a[i + k * a_dim1] = 0.f;
        }
        a[i + i * a_dim1] = 1.f;
        if (i < *n - 1) {
            i__1 = *n;
            for (k = i; k <= i__1; ++k) {
                mtxgcl_((double P_T *)&a[a_offset], s, m, n, &k);
                i__2 = i + 1;
                i__3 = i + 2;
                mtxhst_(v, &ups[i - 1], &bbs[i - 1], s, n, &i__2, &i__3);
                mtxpcl_((double P_T *)&a[a_offset], s, m, n, &k);
            }
        }
    }
    MEMFREE((void *)bbs);
f_1:
    MEMFREE((void *)s);
f_0:
    return;
} /* mtxsv1_ */

