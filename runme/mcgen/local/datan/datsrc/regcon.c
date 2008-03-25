#include <math.h>
#include "datsrc.h"

void LIBFUNC regcon_(double const P_T *x, double const P_T *b,
             double const P_T *t, double const P_T *chi2,
             double const P_T *p, integer const P_T *nr,
             integer const P_T *nf, double P_T *eta, double P_T *coneta)
{
    /* System generated locals */
    integer b_dim1, b_offset, i__1, i__2, i__3;

    /* Local variables */
    integer j, k;
    double fact, d, s, pprime;

    /* Parameter adjustments */
    b_dim1 = *nr;
    b_offset = b_dim1 + 1;
    b -= b_offset;
    --x;

    /* Function Body */
    pprime = (*p + 1) * .5;
    if (*chi2 <= 0.) {
        s = 1.;
        fact = sqstnr_(&pprime);
    } else {
        s = sqrt(*chi2 / (double) (*nf));
        fact = sqstud_(&pprime, nf);
    }
    *eta = 0.;
    *coneta = 0.;
    i__1 = *nr;
    for (j = 1; j <= i__1; ++j) {
        d = b[j + b_dim1];
        if (j > 1) {
            i__2 = j;
            for (k = 2; k <= i__2; ++k) {
                i__3 = k - 1;
                d += b[j + k * b_dim1] * pow(*t, (double)i__3);
            }
        }
        *eta += x[j] * d;
        *coneta += d * d;
    }
    *coneta = fact * s * sqrt(*coneta);
    return;
} /* regcon_ */

