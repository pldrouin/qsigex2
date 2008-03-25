#include "datsrc.h"

void LIBFUNC mincmb_(double P_T *a, double P_T *b, double const P_T *epsiln,
             double (CBFUNC P_T *userfn)(double const P_T *,
             integer const P_T *), double P_T *xmin, double P_T *fmin,
             integer P_T *nstep, double const P_T *x0, double const P_T *xdir,
             integer const P_T *n)
{
    /* System generated locals */
    double d__1;

    /* Local variables */
    integer istep;
    double d, e, p, q, r, u, v, w, x, t2, fu, fv, fw, fx, xm, eps, tol;
    logical parstp;

/* if some input values are not given use defaults */
    /* Parameter adjustments */
    --xdir;
    --x0;

    /* Function Body */
    if (*nstep < 1) {
        *nstep = 1000;
    }
    eps = *epsiln;
    if (eps <= 0.) {
        eps = 1e-12;
    }
/* initialize X at golden section position between A and B */
    x = *a + (*b - *a) * .381966;
/* initialize E,V,W,FX,FV,FW */
    e = 0.;
    v = x;
    w = x;
    fx = minfnd_(&x, &x0[1], &xdir[1], n, userfn);
    fv = fx;
    fw = fx;
    istep = 1;
/* start iteration */
L10:
    if (istep >= *nstep) {
/* prepare exit if step number is too large */
        *nstep = -1;
        *xmin = x;
        *fmin = fx;
        goto L20;
    }
    xm = (*a + *b) * .5;
    tol = eps * abs(x) + 1e-15;
    t2 = tol * 2.;
    if ((d__1 = x - xm, abs(d__1)) > t2 - (*b - *a) * .5) {
        p = 0.;
        q = 0.;
        r = 0.;
        parstp = FALSE_;
        if (abs(e) > tol) {
/* fit parabola */
            r = (x - w) * (fx - fv);
            q = (x - v) * (fx - fw);
            p = (x - v) * q - (x - w) * r;
            q = (q - r) * 2.;
            if (q > 0.) {
                p = -p;
            } else {
                q = -q;
            }
            r = e;
            e = d;
            if (abs(p) < (d__1 = q * .5 * r, abs(d__1)) && p > q * (*a - x) &&
                     p < q * (*b - x)) {
/* use result of parabolic fit */
                d = p / q;
                u = x + d;
                if (u - *a < t2 || *b - u < t2) {
/* make sure that U is not too near to A or B */
                    d = -tol;
                    if (x < xm) {
                        d = tol;
                    }
                }
                parstp = TRUE_;
            }
        }
        if (! parstp) {
/* perform golden section step */
            if (x < xm) {
                e = *b - x;
            } else {
                e = *a - x;
            }
            d = e * .381966;
        }
/* determine point U where function is to be computed */
/* making sure that it is not too close to X */
        if (abs(d) >= tol) {
            u = x + d;
        } else if (d > 0.) {
            u = x + tol;
        } else {
            u = x - tol;
        }
        fu = minfnd_(&u, &x0[1], &xdir[1], n, userfn);
/* update A,B,V,W,X */
        if (fu <= fx) {
            if (u < x) {
                *b = x;
            } else {
                *a = x;
            }
            v = w;
            fv = fw;
            w = x;
            fw = fx;
            x = u;
            fx = fu;
        } else {
            if (u < x) {
                *a = u;
            } else {
                *b = u;
            }
            if (fu <= fw || w == x) {
                v = w;
                fv = fw;
                w = u;
                fw = fu;
            } else if (fu <= fv || v == x || v == w) {
                v = u;
                fv = fu;
            }
        }
        ++istep;
        goto L10;
    } else {
        *xmin = x;
        *fmin = fx;
        *nstep = istep;
    }
L20:
    return;
} /* mincmb_ */

