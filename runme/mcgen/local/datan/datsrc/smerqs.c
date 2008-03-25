#include <math.h>
#include "datsrc.h"

/* Common Block Declarations */

#if defined(__TURBOC__)
extern
#endif
struct {
    double calpha;
} LIBDATA cmsmer_;

void LIBFUNC smerqs_(integer const P_T *k, integer const P_T *id,
             double const P_T *confid, double const P_T *rback,
             double P_T *rsmin, double P_T *rspls, double P_T *rsupr)
{
    /* Table of constant values */

    double c_d1em6 = 1e-6;

    integer kk;
    double alpha, rs0, rs1;

    alpha = 1. - *confid;
/* compute lower limit RSMIN of confidence region */
    if (*k == 0) {
        *rsmin = 0.;
    } else {
        kk = *k;
        cmsmer_.calpha = 1. - alpha * .5;
        rs0 = 0.;
        rs1 = 1.;
        if (*k > 0) {
            rs1 = (double) (*k);
        }
        auxzbr_(&rs0, &rs1, smzrqs_, rback, &kk, id);
        auxzfn_(&rs0, &rs1, rsmin, smzrqs_, rback, &kk, id, &c_d1em6);
    }
    cmsmer_.calpha = alpha * .5;
/* compute upper limit RSPLS of confidence region */
    kk = *k + 1;
    rs0 = 0.;
    rs1 = (double) (*k);
    auxzbr_(&rs0, &rs1, smzrqs_, rback, &kk, id);
    auxzfn_(&rs0, &rs1, rspls, smzrqs_, rback, &kk, id, &c_d1em6);
/* compute upper limit RSUPR of of parameter */
    cmsmer_.calpha = alpha;
    kk = *k + 1;
    rs0 = 0.;
    rs1 = (double) (*k);
    auxzbr_(&rs0, &rs1, smzrqs_, rback, &kk, id);
    auxzfn_(&rs0, &rs1, rsupr, smzrqs_, rback, &kk, id, &c_d1em6);
    return;
} /* smerqs_ */

/* ----------------------------------------------------------------- */
double LIBFUNC smzrqs_(double const P_T *rs, double const P_T *rb,
                      integer const P_T *k, integer const P_T *id)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    double ret_val, d__1, d__2, d__3;

    /* Local variables */
    integer n, nb, ns;
    double den, e, f, g, eb, pb, es, ps;

    n = *k - 1 + *id;
    den = *rs + 1. + *rb;
    ps = *rs / den;
    pb = *rb / den;
    if (pb < 1e-6) {
        den = 1.;
    } else {
        den = scbinm_(k, &n, &pb);
    }
    d__1 = (double) (n + 1);
    g = glngam_(&d__1);
    f = 0.;
    i__1 = *k - 1;
    for (nb = 0; nb <= i__1; ++nb) {
        i__2 = *k - nb - 1;
        for (ns = 0; ns <= i__2; ++ns) {
            if (ns == 0) {
                es = 1.;
            } else {
                es = pow(ps, (double)ns);
            }
            if (nb == 0) {
                eb = 1.;
            } else {
                if (pb < 1e-6) {
                    eb = 0.;
                } else {
                    eb = pow(pb, (double)nb);
                }
            }
            if (n - ns - nb == 0) {
                e = 1.;
            } else {
                d__1 = 1. - ps - pb;
                i__3 = n - ns - nb;
                e = pow(d__1, (double)i__3);
            }
            d__1 = (double) (ns + 1);
            d__2 = (double) (nb + 1);
            d__3 = (double) (n - ns - nb + 1);
            f += exp(g - glngam_(&d__1) - glngam_(&d__2) - glngam_(&d__3)) * 
                    es * eb * e;
        }
    }
    ret_val = cmsmer_.calpha - f / den;
    return ret_val;
} /* smzrqs_ */

