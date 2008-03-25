#include "datsrc.h"

void LIBFUNC auxgrd_(double P_T *x, double P_T *grd, integer const P_T *n,
             integer const P_T *nred, integer const P_T *list,
             double (CBFUNC P_T *func)(double const P_T *,
             integer const P_T *))
{
    /* System generated locals */
    integer i__1;
    double d__1;

    /* Local variables */
    integer i, il;
    double fm, fp, del, arg, sav;

    /* Parameter adjustments */
    --list;
    --grd;
    --x;

    /* Function Body */
    il = 0;
    if (*nred > 0) {
        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
            if (list[i] == 1 || *nred == *n) {
                ++il;
                arg = (d__1 = x[i], abs(d__1));
                if (arg < 1e-9) {
                    arg = 1e-9;
                }
                del = arg * 1e-11;
                sav = x[i];
                x[i] = sav + del;
                fp = (*func)(&x[1], n);
                x[i] = sav - del;
                fm = (*func)(&x[1], n);
                x[i] = sav;
                grd[il] = (fp - fm) / (del + del);
            }
        }
    }
    return;
} /* auxgrd_ */

