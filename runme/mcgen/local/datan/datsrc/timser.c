#include <stdlib.h>
#include <math.h>
#include "datsrc.h"

/* Common Block Declarations */

/*
struct {
    double x[1000], ytmp[1000], etatmp[1000], t[1000];
} dadv03_;

#define dadv03_1 dadv03_
*/

void LIBFUNC timser_(double const P_T *y, integer const P_T *n,
             integer const P_T *k, integer const P_T *l,
             double const P_T *p, double P_T *eta, double P_T *coneta,
             double P_T *F_P(a), double P_T *F_P(ata1),
             double P_T *F_P(ata1at), double P_T *F_P(scrat))
{
    /* Table of constant values */

    double c_dm1 = -1.;
    integer c__1 = 1;

    /* System generated locals */
    integer a_dim1, a_offset, ata1_dim1, ata1_offset, ata1at_dim1,
            ata1at_offset, scrat_dim1, scrat_offset, i__1, i__2, i__3, i__4;
    double d__1;

    /* Local variables */
    integer i, j, i1, i2, i3, l1, ia, ib, k21, nf, is, iadd;
    double a0, etai, seta2, pprime, sy2, talpha;
    double *t, *x, *etatmp, *ytmp;
    unsigned long amem;

#if defined(USEHUGE)
    HFP(double,a); HFP(double,ata1); HFP(double,ata1at); HFP(double,scrat);
#endif

    if ((amem = sizeof(double) * 2 * (*l + 1)) > MAXALLOC) {
        MEMERROR("timser_: t (too large)");
        goto f_0;
    }
    t = (double *)MEMALLOC(amem);
    if (!t) {
        MEMERROR("timser_: t");
        goto f_0;
    }
    if ((amem = sizeof(double) * 2 * (2 * *k + 1)) > MAXALLOC) {
        MEMERROR("timser_: etatmp (too large)");
        goto f_1;
    }
    etatmp = (double *)MEMALLOC(amem);
    if (!etatmp) {
        MEMERROR("timser_: etatmp");
        goto f_1;
    }
    x = t + *l + 1; ytmp = etatmp + 2 * *k + 1;

/* quantile of Student's distribution */
    /* Parameter adjustments */
    --y;
    --coneta;
    --eta;
    scrat_dim1 = *l + 1;
    scrat_offset = scrat_dim1 + 1;
    scrat -= scrat_offset;
    ata1at_dim1 = *l + 1;
    ata1at_offset = ata1at_dim1 + 1;
    ata1at -= ata1at_offset;
    ata1_dim1 = *l + 1;
    ata1_offset = ata1_dim1 + 1;
    ata1 -= ata1_offset;
    a_dim1 = (*k << 1) + 1;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    pprime = (*p + 1) * .5;
    nf = (*k << 1) - *l;
    talpha = sqstud_(&pprime, &nf);
/* compute matrices depending only on K and L */
    k21 = (*k << 1) + 1;
    l1 = *l + 1;
    i__1 = k21;
    for (i = 1; i <= i__1; ++i) {
        i__2 = l1;
        for (j = 1; j <= i__2; ++j) {
            if (j == 1) {
                a[i + j * a_dim1] = -1.;
            } else {
                a[i + j * a_dim1] = a[i + (j - 1) * a_dim1] *
                                    (double) (i - *k - 1);
            }
        }
    }
    mtxmat_((double P_T *)&a[a_offset], (double P_T *)&a[a_offset],
           (double P_T *)&ata1[ata1_offset], &l1, &k21, &l1);
    mtxchi_((double P_T *)&ata1[ata1_offset],
           (double P_T *)&scrat[scrat_offset], &l1);
    mtxmbt_((double P_T *)&ata1[ata1_offset], (double P_T *)&a[a_offset],
           (double P_T *)&ata1at[ata1at_offset], &l1, &l1, &k21);
    mtxmsc_((double P_T *)&ata1at[ata1at_offset],
           (double P_T *)&ata1at[ata1at_offset], &c_dm1, &l1, &k21);
/* loop over inner part of time series */
    ia = (*k << 1) + 1;
    ib = *n;
    i__1 = ib;
    for (i = ia; i <= i__1; ++i) {
/* moving averages and confidence limits for inner part */
        i__2 = i - ia + 1;
        mtxgsm_(&y[1], ytmp, n, &c__1, &k21, &c__1, &i__2, &c__1);
        mtxmlt_((double P_T *)&ata1at[ata1at_offset], ytmp, x, &l1, &k21, &c__1);
        eta[i] = x[0];
        mtxmlt_((double P_T *)&a[a_offset], x, etatmp, &k21, &l1, &c__1);
        mtxadd_(ytmp, etatmp, etatmp, &k21, &c__1);
        mtxmat_(etatmp, etatmp, &sy2, &c__1, &k21, &c__1);
        sy2 /= (double) nf;
        a0 = sqrt((d__1 = ata1[ata1_dim1 + 1], abs(d__1)));
        coneta[i] = a0 * sqrt(sy2) * talpha;
/* moving averages and confidence limits for end sections */
        if (i == ia || i == ib) {
            if (i == ia) {
                iadd = ia;
                is = -1;
            } else {
                iadd = ib;
                is = 1;
            }
            i__2 = *k << 1;
            for (i1 = 1; i1 <= i__2; ++i1) {
                j = is * i1;
                i__3 = l1;
                for (i2 = 1; i2 <= i__3; ++i2) {
                    i__4 = i2;
                    for (i3 = 1; i3 <= i__4; ++i3) {
                        if (i3 == 1) {
                            t[i2 - 1] = 1.;
                        } else {
                            t[i2 - 1] *= j;
                        }
                    }
                }
                mtxmbt_((double P_T *)&ata1[ata1_offset], t,
                       (double P_T *)&scrat[scrat_offset], &l1, &l1, &c__1);
                mtxmlt_(t, (double P_T *)&scrat[scrat_offset], &seta2, &c__1,
                       &l1, &c__1);
                seta2 = sy2 * seta2;
                mtxmlt_(t, x, &etai, &c__1, &l1, &c__1);
                coneta[iadd + j] = sqrt((abs(seta2))) * talpha;
                eta[iadd + j] = etai;
            }
        }
    }
    MEMFREE((void *)etatmp);
f_1:
    MEMFREE((void *)t);
f_0:
    return;
} /* timser_ */

