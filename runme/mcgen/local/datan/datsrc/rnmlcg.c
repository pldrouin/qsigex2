#include "datsrc.h"

static void LOCLIBFUNC rnmlcg__0_(int n__, double P_T *u, integer P_T *n,
                                 integer P_T *iseed)
{
    /* Initialized data */

    static integer ix = 123456;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i, k;

    /* Parameter adjustments */
    if (u) {
        --u;
        }

    /* Function Body */
    switch(n__) {
        case 1: goto L_rnmsin_;
        case 2: goto L_rnmsot_;
        }

/* Compute N random numbers */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        k = ix / 53668;
        ix = (ix - k * 53668) * 40014 - k * 12211;
        if (ix < 0) {
            ix += 2147483563;
        }
        u[i] = (double) ix * 4.656613e-10;
    }
    return;
/* Input of seed */

L_rnmsin_:
    ix = *iseed;
    return;
/* Output of seed */

L_rnmsot_:
    *iseed = ix;
    return;
} /* rnmlcg_ */

void LIBFUNC rnmlcg_(double P_T *u, integer const P_T *n)
{
    integer nc = *n;
    rnmlcg__0_(0, u, &nc, (integer *)0);
    return;
    }

void LIBFUNC rnmsin_(integer const P_T *iseed)
{
    integer iseedc = *iseed;
    rnmlcg__0_(1, (double *)0, (integer *)0, &iseedc);
    return;
    }

void LIBFUNC rnmsot_(integer P_T *iseed)
{
    rnmlcg__0_(2, (double *)0, (integer *)0, iseed);
    return;
    }

