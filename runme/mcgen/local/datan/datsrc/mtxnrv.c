#include <math.h>
#include "datsrc.h"

void LIBFUNC mtxnrv_(double const P_T *F_P(u), double P_T *s,
             integer const P_T *n)
{
    /* Local variables */

#if defined(USEHUGE)
    HFP(double,u);
#endif

    mtxdot_(u, u, s, n);
    *s = sqrt((abs(*s)));
    return;
} /* mtxnrv_ */

