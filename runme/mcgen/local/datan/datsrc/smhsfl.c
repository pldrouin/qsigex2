#include "datsrc.h"

void LIBFUNC smhsfl_(double P_T *hist, double const P_T *x0,
             double const P_T *delx, integer const P_T *nx,
             double const P_T *xin, double const P_T *weight)
{
    integer n;

    /* Parameter adjustments */
    --hist;

    /* Function Body */
    n = (integer) ((*xin - *x0) / *delx) + 1;
    if (n > 0 && n <= *nx) {
        hist[n] += *weight;
    }
    return;
} /* smhsfl_ */

