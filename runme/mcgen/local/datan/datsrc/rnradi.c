#include <math.h>
#include "datsrc.h"

void LIBFUNC rnradi_(double const P_T *a, double const P_T *tau1,
                    double const P_T *tau2, double P_T *t)
{
    /* Table of constant values */

    integer c__3 = 3;

    /* Local variables */
    double r[3];

    rnecuy_(r, &c__3);
    if (r[0] < *a) {
/* Mean life is TAU1 */
        *t = -(*tau1) * log(r[1]);
    } else {
/* Mean life is TAU2 */
        *t = -(*tau2) * log(r[2]);
    }
    return;
} /* rnradi_ */

