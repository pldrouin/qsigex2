#include <math.h>
#include "datsrc.h"

double LIBFUNC sdftst_(double const P_T *x, integer const P_T *nf1,
                      integer const P_T *nf2)
{
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double al, af1, af2, af12, af22, afs2;

    af1 = (double) (*nf1);
    af2 = (double) (*nf2);
    af12 = af1 * .5;
    af22 = af2 * .5;
    afs2 = af12 + af22;
    if (*x <= 0.) {
        ret_val = 0.;
    } else {
        al = af12 * log(af1 / af2) - afs2 * log(af1 * *x / af2 + 1.) +
             (af12 - 1.) * log(*x) + glngam_(&afs2) - glngam_(&af12) -
             glngam_(&af22);
        ret_val = exp(al);
    }
    return ret_val;
} /* sdftst_ */

