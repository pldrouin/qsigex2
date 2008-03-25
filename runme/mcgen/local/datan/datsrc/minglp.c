#include <math.h>
#include "datsrc.h"

/* Common Block Declarations */

#if defined(__TURBOC__)
extern
#endif
struct cmingh_ {
    double t[100], hist[100], deltat;
    integer nt, nevent;
} LIBDATA cmingh_;

double LIBFUNC minglp_(double const P_T *x, integer const P_T *n)
{
    /* System generated locals */
    integer i__1;
    double ret_val, d__1;

    /* Local variables */
    integer i, ni;
    double alam, alnlam, alnni, fnorm, gi;

    /* Parameter adjustments */
    --x;

    /* Function Body */
    fnorm = (double) cmingh_.nevent * cmingh_.deltat;
    ret_val = 0.;
    i__1 = cmingh_.nt;
    for (i = 1; i <= i__1; ++i) {
/* GI is the value of the probability density of the population */
/* at T(I) (by replacing the RHS of the following statement it can */
/* be changed from normal to any desired distribution) */
        gi = sdnorm_(&cmingh_.t[i - 1], &x[1], &x[2]);
/* normalize to number of events in sample */
        gi = fnorm * gi;
        alam = gi;
        if (alam > 0.) {
            ni = nint(cmingh_.hist[i - 1]);
            alnlam = log(alam);
            d__1 = (double) (ni + 1);
            alnni = glngam_(&d__1);
            ret_val = ret_val + alnni - cmingh_.hist[i - 1] * alnlam + alam;
        }
    }
    return ret_val;
} /* minglp_ */

