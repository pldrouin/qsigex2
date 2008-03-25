#include "datsrc.h"

void LIBFUNC smhsin_(double P_T *hist, double const P_T *x0,
                    double const P_T *delx, integer const P_T *nx)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i;

    /* Parameter adjustments */
    --hist;

    /* Function Body */
    i__1 = *nx;
    for (i = 1; i <= i__1; ++i) {
        hist[i] = 0.f;
    }
    return;
} /* smhsin_ */

