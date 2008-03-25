#include <stdio.h>
#include "datsrc.h"
#if defined(WINDOWS)
#  include "datandll.h"
#endif

void LIBFUNC mtxwrt_(double const P_T *F_P(a), integer const P_T *m,
             integer const P_T *n)
{
#if defined(WINDOWS)
#  define OUTSLEN 82
    char outs[OUTSLEN];
    integer l_outs;
#endif

    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    integer i, j;

#if defined(USEHUGE)
    HFP(double,a); 
#endif

    /* Parameter adjustments */
    a_dim1 = *m;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    i__1 = *m;
    for (i = 1; i <= i__1; ++i) {
#if defined(WINDOWS)
        outs[0] = 0;
        l_outs = 0;
#endif
        i__2 = *n;
        for (j = 1; j <= i__2; ++j) {
#if defined(WINDOWS)
            if (j > 1 && j % 4 == 1 || l_outs > OUTSLEN-21) {
                wnwrst(outs, &l_outs);
                l_outs = 0;
            }
            sprintf(outs+l_outs,"%20.13lG",a[i + j * a_dim1]);
            l_outs = strlen(outs);
#else
            (void)printf("%20.13lG",a[i + j * a_dim1]);
            if (j % 4 == 0)
                (void)printf("\n");
#endif
        }
#if defined(WINDOWS)
        wnwrst(outs, &l_outs);
#else
        if (i__2 % 4 != 0) 
            (void)printf("\n");
#endif
    }
    return;
} /* mtxwrt_ */

