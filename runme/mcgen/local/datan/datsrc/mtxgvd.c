#include <math.h>
#include "datsrc.h"

void LIBFUNC mtxgvd_(double const P_T *v1, double const P_T *v2,
             double P_T *c, double P_T *s)
{
    /* Local variables */
    double q, w, a1, a2;

    a1 = abs(*v1);
    a2 = abs(*v2);
    if (a1 > a2) {
        w = *v2 / *v1;
        q = sqrt(w * w + 1.f);
        *c = 1.f / q;
        if (*v1 < 0.f) {
            *c = -(*c);
        }
        *s = *c * w;
    } else {
        if (*v2 != 0.f) {
            w = *v1 / *v2;
            q = sqrt(w * w + 1.f);
            *s = 1.f / q;
            if (*v2 < 0.f) {
                *s = -(*s);
            }
            *c = *s * w;
        } else {
            *c = 1.f;
            *s = 0.f;
        }
    }
    return;
} /* mtxgvd_ */

