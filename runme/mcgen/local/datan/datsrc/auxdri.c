#include <stdlib.h>
#include "datsrc.h"

#if defined(MAKELIB)
extern struct {
    double dx[10];
    double w[30]; /* was [10][3] */
    logical lev[10];
} cmauxc;
#endif

void LIBFUNC auxdri_(double (CBFUNC P_T *f)(double const P_T *,
             integer const P_T *, double const P_T *), double P_T *x,
             double const P_T *tt, integer const P_T *ny,
             integer const P_T *nr, integer const P_T *nred,
             integer const P_T *list, double P_T *aa, logical P_T *ok)
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
    integer aa_dim1, aa_offset, i__1, i__2, i__3;
    double d__1;

    /* Local variables */
    integer i, k, m, ir, ix, iy;
    double fminus, fplus, del, xsav, h;
    logical lmt;
    /*double a[10], t[100] (* was [10][10] *);*/
    double *a, *t;

    a = (double *)MEMALLOC(sizeof(double)*(size_t)(100+10));
    if (!a) {
        MEMERROR("auxdri_");
        *ok = FALSE_;
        return;
    }
    t = a + 10;

/* Computes matrix of derivatives */
/* Based on H. Rutishauser, Ausdehnung des Rombergschen Prinzips */
/* (Extension of Romberg's Principle), Numer. Math. 5 (1963) 48-54 */
/* Program based on routine DERIV by K.S.Koelbig in */
/* CERN Computer Center Program Library, CERN, Geneva */
    /* Parameter adjustments */
    --tt;
    --list;
    --x;
    aa_dim1 = *ny;
    aa_offset = aa_dim1 + 1;
    aa -= aa_offset;

    /* Function Body */
    *ok = TRUE_;
    i__1 = *ny;
    for (iy = 1; iy <= i__1; ++iy) {
        ix = 0;
        i__2 = *nr;
        for (ir = 1; ir <= i__2; ++ir) {
            if (list[ir] != 0) {
                ++ix;
                del = 10.;
                xsav = x[ir];
                for (i = 1; i <= 10; ++i) {
                    del *= .1;
                    if (i == 10 || (d__1 = xsav + del * dx[9] - xsav,
                                    abs(d__1)) < 5e-8) {
                        *ok = FALSE_;
                        goto L99;
                    }
                    for (k = 0; k <= 9; ++k) {
                        h = del * dx[k];
                        x[ir] = xsav + h;
                        fplus = (*f)(&x[1], nr, &tt[iy]);
                        x[ir] = xsav - h;
                        fminus = (*f)(&x[1], nr, &tt[iy]);
                        x[ir] = xsav;
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
                        lmt = lmt && (h <= 1e-10 || abs(h) <= (d__1 = a[k], 
                                abs(d__1)) * 5e-8 + 1e-10);
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
                            t[k + m * 10] = w[m - 1] * t[k + 1 + (m - 1) * 10]
                                            - w[m] * t[k + (m - 1) * 10];
                        } else if (lev[k]) {
                            t[k + m * 10] = w[m + 9] * t[k + 1 + (m - 1) * 10]
                                            - w[m + 10] * t[k + (m - 1) * 10];
                        } else {
                            t[k + m * 10] = w[m + 19] * t[k + 1 + (m - 1) * 10]
                                            - w[m + 20] * t[k + (m - 1) * 10];
                        }
                    }
                }
                aa[iy + ix * aa_dim1] = t[90];
            }
        }
    }
L99:
    MEMFREE((void *)a);
    return;
} /* auxdri_ */

