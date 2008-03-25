#include <stdlib.h>
#include "datsrc.h"

#if defined(MAKELIB)
extern struct {
    double dx[10];
    double w[30]; /* was [10][3] */
    logical lev[10];
} cmauxc;
#endif

void LIBFUNC auxdrg_(double P_T *x, double P_T *eta, integer const P_T *mm,
             integer const P_T *n, integer const P_T *nr,
             integer const P_T *nred, integer const P_T *list,
             double P_T *e, logical P_T *ok)
{
#if !defined(MAKELIB)
    /* Initialized data */

    static double dx[10] = { .0256,.0192,.0128,.0096,.0064,.0048,.0032,
            .0024,.0016,.0012 };
    static logical lev[10] = { TRUE_,FALSE_,TRUE_,FALSE_,TRUE_,FALSE_,TRUE_,
            FALSE_,TRUE_,FALSE_ };
    static double w[30] /* was [10][3] */ = { 0.0,1.3333333333333333,
            .33333333333333333,1.0666666666666667,.066666666666666667,
            1.0158730158730159,.015873015873015873,1.003921568627451,
            .0039215686274509804,0.0,2.2857142857142857,1.2857142857142857,
            1.1636363636363636,.16363636363636364,1.0364372469635628,
            .036437246963562753,1.0088669950738916,.0088669950738916256,
            1.0022021042329337,.0022021042329336922,1.8,.8,1.125,.125,
            1.0285714285714286,.028571428571428571,1.006993006993007,
            .006993006993006993,1.0017391304347826,.0017391304347826087 };
#else
    double *w = cmauxc.w, *dx = cmauxc.dx;
    logical *lev = cmauxc.lev;
#endif

    /* System generated locals */
    integer e_dim1, e_offset, i__1, i__2, i__3;
    double d__1;

    /* Local variables */
    integer i, k, l, m, i2, il, im, iy;
    double fminus, fplus, del, xsav, h;
    logical lmt, lx;
    /* double a[10], t[100] (* was [10][10] *);*/
    double *a, *t;

    a = (double *)MEMALLOC(sizeof(double)*(100+10));
    if (!a) {
        MEMERROR("auxdrg_");
        *ok = FALSE_;
        return;
    }
    t = a + 10;

/* Computes the derivative f'(x) of f(x) at x = X. Based on */
/* H. Rutishauser, Ausdehnung des Rombergschen Prinzips */
/* (Extension of Romberg's Principle), Numer. Math. 5 (1963) 48-54 */
    /* Parameter adjustments */
    --eta;
    --list;
    --x;
    e_dim1 = *mm;
    e_offset = e_dim1 + 1;
    e -= e_offset;

    /* Function Body */
    *ok = TRUE_;
    l = *nr + *n;
    i__1 = *mm;
    for (im = 1; im <= i__1; ++im) {
        i2 = 0;
        i__2 = l;
        for (il = 1; il <= i__2; ++il) {
            del = 10.;
            if (il <= *nr) {
                if (list[il] != 0) {
                    ++i2;
                    lx = TRUE_;
                    xsav = x[il];
                } else {
                    goto L80;
                }
            } else {
                ++i2;
                lx = FALSE_;
                iy = il - *nr;
                xsav = eta[iy];
            }
            for (i = 1; i <= 10; ++i) {
                del *= .1;
                if (i == 10 || (d__1 = xsav + del * dx[9] - xsav, abs(d__1)) <
                         5e-8) {
                    *ok = FALSE_;
                    goto L99;
                }
                for (k = 0; k <= 9; ++k) {
                    h = del * dx[k];
                    if (lx) {
                        x[il] = xsav + h;
                    } else {
                        eta[iy] = xsav + h;
                    }
                    fplus = lsqgfn_(&eta[1], &x[1], n, nr, &im);
                    if (lx) {
                        x[il] = xsav - h;
                    } else {
                        eta[iy] = xsav - h;
                    }
                    fminus = lsqgfn_(&eta[1], &x[1], n, nr, &im);
                    if (lx) {
                        x[il] = xsav;
                    } else {
                        eta[iy] = xsav;
                    }
                    t[k] = (fplus - fminus) / (h + h);
                    a[k] = t[k];
                }
                if (a[0] >= a[9]) {
                    for (k = 0; k <= 9; ++k) {
                        a[k] = -a[k];
                    }
                }
                lmt = TRUE_;
                for (k = 1; k <= 9; ++k) {
                    h = a[k - 1] - a[k];
                    lmt = lmt && (h <= 1e-10 || abs(h) <=
                                  (d__1 = a[k], abs(d__1)) * 5e-8 + 1e-10);
                }
                if (lmt) {
                    goto L50;
                }
            }
L50:
            for (m = 1; m <= 9; ++m) {
                i__3 = 9 - m;
                for (k = 0; k <= i__3; ++k) {
                    if (lev[m]) {
                        t[k + m * 10] = w[m - 1] * t[k + 1 + (m - 1) * 10] -
                                w[m] * t[k + (m - 1) * 10];
                    } else if (lev[k]) {
                        t[k + m * 10] = w[m + 9] * t[k + 1 + (m - 1) * 10] -
                                w[m + 10] * t[k + (m - 1) * 10];
                    } else {
                        t[k + m * 10] = w[m + 19] * t[k + 1 + (m - 1) * 10] -
                                w[m + 20] * t[k + (m - 1) * 10];
                    }
                }
            }
            e[im + i2 * e_dim1] = t[90];
L80:
            ;
        }
    }
L99:
    MEMFREE((void *)a);
    return;
} /* auxdrg_ */

