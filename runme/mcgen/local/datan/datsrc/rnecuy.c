#include "datsrc.h"

static void LOCLIBFUNC rnecuy__0_(int n__, double P_T *u, integer P_T *n,
                              integer P_T *iseed1, integer P_T *iseed2)
{
    /* Initialized data */

    static integer ix1 = 123456;
    static integer ix2 = 654321;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i, k, iz;

    /* Parameter adjustments */
    if (u) {
        --u;
        }

    /* Function Body */
    switch(n__) {
        case 1: goto L_rne2in_;
        case 2: goto L_rne2ot_;
        }

    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
/* Produce integer random number X1 from first MLCG */
        k = ix1 / 53668;
        ix1 = (ix1 - k * 53668) * 40014 - k * 12211;
        if (ix1 < 0) {
            ix1 += 2147483563;
        }
/* Produce integer random number X2 from second MLCG */
        k = ix2 / 52774;
        ix2 = (ix2 - k * 52774) * 40692 - k * 2791;
        if (ix2 < 0) {
            ix2 += 2147483399;
        }
/* Combine */
        iz = ix1 - ix2;
        if (iz < 1) {
            iz += 2147483562;
        }
/* Normalize and transform to floating point */
        u[i] = (double) iz * 4.656613e-10;
    }
    return;
/* Input of seed */

L_rne2in_:
    ix1 = *iseed1;
    ix2 = *iseed2;
    return;
/* Output of seed */

L_rne2ot_:
    *iseed1 = ix1;
    *iseed2 = ix2;
    return;
} /* rnecuy_ */

void LIBFUNC rnecuy_(double P_T *u, integer const P_T *n)
{
    integer nc = *n;
    rnecuy__0_(0, u, &nc, (integer *)0, (integer *)0);
    return;
    }

void LIBFUNC rne2in_(integer const P_T *iseed1, integer const P_T *iseed2)
{
    integer iseed1c = *iseed1, iseed2c = *iseed2;
    rnecuy__0_(1, (double *)0, (integer *)0, &iseed1c, &iseed2c);
    return;
    }

void LIBFUNC rne2ot_(integer P_T *iseed1, integer P_T *iseed2)
{
    rnecuy__0_(2, (double *)0, (integer *)0, iseed1, iseed2);
    return;
    }

