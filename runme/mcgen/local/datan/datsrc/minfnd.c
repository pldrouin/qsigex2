#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

double LIBFUNC minfnd_(double const P_T *a, double const P_T *x0,
               double const P_T *xdir, integer const P_T *n,
               double (CBFUNC P_T *userfn)(double const P_T *,
               integer const P_T *))
{
    /* System generated locals */
    integer i__1;
    double ret_val;

    /* Local variables */
    integer i;
    /*double xline[50];*/
    double *xline;
    unsigned long amem;

    if ((amem = sizeof(double) * *n) > MAXALLOC) {
        MEMERROR("minfnd_ (too large)");
        return 0.; /*here*/
    }
    xline = (double *)MEMALLOC(amem);
    if (!xline) {
        MEMERROR("minfnd_");
        return 0.; /*here*/
    }

    /* Parameter adjustments */
    --xdir;
    --x0;

    /* Function Body */
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
        xline[i - 1] = x0[i] + *a * xdir[i];
    }
    ret_val = (*userfn)(xline, n);
    MEMFREE((void *)xline);
    return ret_val;
} /* minfnd_ */

