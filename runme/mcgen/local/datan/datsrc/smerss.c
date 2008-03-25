#include <math.h>
#include "datsrc.h"

/* Common Block Declarations */

struct {
    double calpha, cadnom;
} LIBDATA cmsmer_;

void LIBFUNC smerss_(integer const P_T *k, double const P_T *confid,
             double const P_T *alback, double P_T *alsmin,
             double P_T *alspls, double P_T *alsupr)
{
    /* Table of constant values */

    double c_d1em6 = 1e-6;

    integer ndum, kk;
    double alpha, als0, als1;

    alpha = 1. - *confid;
    if (*alback <= 0.) {
        cmsmer_.cadnom = 1.;
    }
/* compute lower limit ALSMIN of confidence region */
    if (*k == 0) {
        *alsmin = 0.;
    } else {
        kk = *k;
        if (*alback > 0.) {
            cmsmer_.cadnom = scpois_(&kk, alback);
        }
        cmsmer_.calpha = 1. - alpha * .5;
        als0 = 0.;
        als1 = 1.;
        if (*k > 0) {
            als1 = (double) (*k);
        }
        auxzbr_(&als0, &als1, smzrss_, alback, &kk, &ndum);
        auxzfn_(&als0, &als1, alsmin, smzrss_, alback, &kk, &ndum, &c_d1em6);
    }
    cmsmer_.calpha = alpha * .5;
/* compute upper limit ALSPLS of confidence region */
    kk = *k + 1;
    if (*alback > 0.) {
        cmsmer_.cadnom = scpois_(&kk, alback);
    }
    als0 = 0.;
    als1 = (double) (*k);
    auxzbr_(&als0, &als1, smzrss_, alback, &kk, &ndum);
    auxzfn_(&als0, &als1, alspls, smzrss_, alback, &kk, &ndum, &c_d1em6);
/* compute upper limit ALSUPR of of parameter */
    cmsmer_.calpha = alpha;
    kk = *k + 1;
    als0 = 0.;
    als1 = (double) (*k);
    auxzbr_(&als0, &als1, smzrss_, alback, &kk, &ndum);
    auxzfn_(&als0, &als1, alsupr, smzrss_, alback, &kk, &ndum, &c_d1em6);
    return;
} /* smerss_ */

/* ----------------------------------------------------------------- */
double LIBFUNC smzrss_(double const P_T *als, double const P_T *alback,
                      integer const P_T *k, integer const P_T *ndum)
{
    /* System generated locals */
    double ret_val, d__1;

    /* Local variables */

    d__1 = *als + *alback;
    ret_val = cmsmer_.calpha - scpois_(k, &d__1) / cmsmer_.cadnom;
    return ret_val;
} /* smzrss_ */

