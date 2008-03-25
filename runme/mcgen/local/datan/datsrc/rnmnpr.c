#include <stdlib.h>
#include <limits.h>
#include "datsrc.h"

void LIBFUNC rnmnpr_(double P_T *c, double P_T *dplus, integer P_T *n)
{
    /* Table of constant values */

    double c_d0 = 0.;

    /* System generated locals */
    integer c_dim1, c_offset, dplus_dim1, dplus_offset;

    /* Local variables */
    /*
    double b[100]       (* was [10][10] *),
           d[100]       (* was [10][10] *),
           r[10];
    */
    logical ok;
    double *b, *d, *r;
    unsigned long amem;

    if ((amem = sizeof(double) * *n * (2 * *n + 1)) > MAXALLOC) {
        MEMERROR("rnmnpr_ (too large)");
        goto f_0;
    }
    b = (double *)MEMALLOC(amem);
    if (!b) {
        MEMERROR("rnmnpr_");
        goto f_0;
    }
    d = b + *n * *n; r = d + *n * *n;

/* prepares for generation of random numbers from multivaraiate normal */
    /* Parameter adjustments */
    dplus_dim1 = *n;
    dplus_offset = dplus_dim1 + 1;
    dplus -= dplus_offset;
    c_dim1 = *n;
    c_offset = c_dim1 + 1;
    c -= c_offset;

    /* Function Body */
    mtxtra_(&c[c_offset], b, n, n);
    mtxchi_(b, d, n);
    mtxchl_(b, d, n);
    mtxunt_(b, n);
    mtxsvd_(d, b, &dplus[dplus_offset], r, n, n, n, &c_d0, &ok);
    MEMFREE((void *)b);
f_0:
    return;
} /* rnmnpr_ */

