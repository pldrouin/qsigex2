#include "datsrc.h"

void LIBFUNC mingld_(double P_T *a, double P_T *b, double P_T *c,
             double const P_T *epsiln,
             double (CBFUNC P_T *userfn)(double const P_T *,
             double const P_T *, double const P_T *, integer const P_T *),
             double P_T *xmin, double P_T *ymin, integer P_T *nstep,
             double const P_T *x0, double const P_T *xdir,
             integer const P_T *n)
{
    /* System generated locals */
    double d__1, d__2;

    /* Local variables */
    integer istep;
    double s, x, y, eps;

    eps = *epsiln;
    if (eps <= 0.f) {
        eps = 1e-8;
    }
    if (*nstep < 1) {
        *nstep = 1000;
    }
    s = (*userfn)(b, x0, xdir, n);
    istep = 0;
L10:
    ++istep;
    if ((d__1 = *b - *a, abs(d__1)) > (d__2 = *c - *b, abs(d__2))) {
        x = *a + (*b - *a) * .61803;
        y = (*userfn)(&x, x0, xdir, n);
        if (y < s) {
            *c = *b;
            *b = x;
            s = y;
        } else {
            *a = x;
        }
    } else {
        x = *b + (*c - *b) * .61803;
        y = (*userfn)(&x, x0, xdir, n);
        if (y < s) {
            *a = *b;
            *b = x;
            s = y;
        } else {
            *c = x;
        }
    }
    if ((d__1 = *c - *a, abs(d__1)) > eps * abs(*a) + 1e-15 &&
          istep <= *nstep) {
        goto L10;
    }
    if (istep > *nstep) {
        *nstep = -1;
    } else {
        *nstep = istep;
    }
    *xmin = *b;
    *ymin = *c;
    return;
} /* mingld_ */

