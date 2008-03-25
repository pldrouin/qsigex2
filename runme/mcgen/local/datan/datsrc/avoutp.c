#include <stdio.h>
#include <string.h>
#include <math.h>
#include "datsrc.h"
#if defined(WINDOWS)
#  include "datandll.h"
#endif

void LIBFUNC avoutp_(double const P_T *x, integer const P_T *ni,
             integer const P_T *nj, integer const P_T *nk,
             double const P_T *q, double const P_T *s,
             double const P_T *f, integer const P_T *ndf,
             double const P_T *alpha, integer const P_T *ntype)
{
    /* Initialized data */
#if defined(WINDOWS)
#  define OUTSLEN 128
    static char outs[OUTSLEN];
	static integer l_outs;
    static char *fmt80 =
                      "   %4s  %10.2lf     %3li     %10.2lf%10.5lf%15.12lf";
#else
    static char *fmt80 =
                      "   %4s  %10.2lf     %3li     %10.2lf%10.5lf%15.12lf\n";
#endif
    static char *source[6] = {"A   ", "B   ", "INT.", "B(A)", "W   ", "TTL."};

    /* System generated locals */
    integer x_dim1, x_dim2, x_offset, i__1, i__2, i__3;

    /* Local variables */
    integer i, j, k, l;

    /* Parameter adjustments */
    x_dim1 = *ni;
    x_dim2 = *nj;
    x_offset = x_dim1 * (x_dim2 + 1) + 1;
    x -= x_offset;
    --q;
    --s;
    --f;
    --ndf;
    --alpha;

    /* Function Body */
#if defined(WINDOWS)
    sprintf(outs,"%s\n", "           DATA");
	l_outs = strlen(outs);
    wnwrst(outs, &l_outs);
	l_outs = 0;
#else
    (void)printf("%s\n\n", "           DATA");
#endif
    i__1 = *ni;
    for (i = 1; i <= i__1; ++i) {
        i__2 = *nj;
        for (j = 1; j <= i__2; ++j) {
#if defined(WINDOWS)
            sprintf(outs+l_outs," A(%2li), B(%2li) ", i, j);
			l_outs = strlen(outs);
#else
            (void)printf(" A(%2li), B(%2li) ", i, j);
#endif
            i__3 = *nk;
            for (k = 1; k <= i__3; ++k) {
#if defined(WINDOWS)
				if (l_outs > OUTSLEN-11) {
					wnwrst(outs, &l_outs);
					l_outs = 0;
				}
                sprintf(outs+l_outs,"%10.2lf", x[i + (j + k * x_dim2) * x_dim1]);
			    l_outs = strlen(outs);
#else
                (void)printf("%10.2lf", x[i + (j + k * x_dim2) * x_dim1]);
#endif
            }
#if defined(WINDOWS)
            wnwrst(outs, &l_outs);
			l_outs = 0;
#else
            (void)printf("\n");
#endif
        }
    }
#if defined(WINDOWS)
	l_outs = 148;
    wnwrst("\n\n\n          Analysis of Variance Table\n\n"
           " Source      Sum of    Degrees of   Mean     F Ratio   Alpha"
           "\n             Squares   Freedom      Square\n\n", &l_outs);
    sprintf(outs, fmt80, source[0],q[1],ndf[1],s[1],f[1],alpha[1]);
	l_outs = strlen(outs);
    wnwrst(outs, &l_outs);
#else
    (void)printf("\n\n\n          Analysis of Variance Table\n\n"
                 " Source      Sum of    Degrees of   Mean     F Ratio   Alpha"
                 "\n             Squares   Freedom      Square\n\n\n");
    (void)printf(fmt80, source[0],q[1],ndf[1],s[1],f[1],alpha[1]);
#endif
    if (*ntype == 0) {
        for (l = 2; l <= 3; ++l) {
#if defined(WINDOWS)
            sprintf(outs, fmt80, source[l-1],q[l],ndf[l],s[l],f[l],alpha[l]);
			l_outs = strlen(outs);
			wnwrst(outs, &l_outs);
#else
            (void)printf(fmt80, source[l-1],q[l],ndf[l],s[l],f[l],alpha[l]);
#endif
        }
    } else {
#if defined(WINDOWS)
        sprintf(outs, fmt80, source[3],q[4],ndf[4],s[4],f[4],alpha[4]);
		l_outs = strlen(outs);
		wnwrst(outs, &l_outs);
#else
        (void)printf(fmt80, source[3],q[4],ndf[4],s[4],f[4],alpha[4]);
#endif
    }
    for (l = 5; l <= 6; ++l) {
#if defined(WINDOWS)
        sprintf(outs, "   %4s  %10.2lf     %3li     %10.2lf",
                     source[l-1],q[l],ndf[l],s[l]);
		l_outs = strlen(outs);
		wnwrst(outs, &l_outs);
#else
        (void)printf("   %4s  %10.2lf     %3li     %10.2lf\n",
                     source[l-1],q[l],ndf[l],s[l]);
#endif
    }
    return;
} /* avoutp_ */

