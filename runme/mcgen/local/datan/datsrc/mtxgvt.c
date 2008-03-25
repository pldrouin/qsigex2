#include "datsrc.h"

void LIBFUNC mtxgvt_(double P_T *z1, double P_T *z2, double const P_T *c,
             double const P_T *s)
{
    double w;

    w = *z1 * *c + *z2 * *s;
    *z2 = -(*z1) * *s + *z2 * *c;
    *z1 = w;
    return;
} /* mtxgvt_ */

