#include <math.h>
#include "datsrc.h"

void LIBFUNC rnstnr_(double P_T *r, integer const P_T *n)
{
    /* Table of constant values */

    integer c__2 = 2;

    /* Local variables */
    integer i;
    double root, s, u[2], v1, v2;

    /* Parameter adjustments */
    --r;

    /* Function Body */
    i = 1;
L10:
    rnecuy_(u, &c__2);
    v1 = u[0] * 2.f - 1.f;
    v2 = u[1] * 2.f - 1.f;
    s = v1 * v1 + v2 * v2;
    if (s >= 1.f) {
        goto L10;
    }
    root = sqrt(log(s) * -2.f / s);
    r[i] = v1 * root;
    if (i < *n) {
        r[i + 1] = v2 * root;
    }
    i += 2;
    if (i <= *n) {
        goto L10;
    }
    return;
} /* rnstnr_ */

