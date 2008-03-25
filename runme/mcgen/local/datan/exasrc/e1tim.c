#include <stdio.h>
#include "datsrc.h"

main(void)
{
    /* Local variables */ /* Initialized data */
    static integer i, k, l, n;
    static double y[36] = { 38.7,50.3,45.6,46.4,43.7,42.,21.8,21.8,51.3,
	      39.5,26.9,23.2,19.8,24.4,17.1,29.3,43.,35.9,19.6,33.2,38.8,35.3,
	      23.4,14.9,15.3,17.7,16.5,6.6,9.5,9.1,3.1,9.3,4.7,6.1,7.4,15.1 },
           a[126] /* was [21][6] */, p, scrat[36] /* was [6][6] */,
           ata1at[126] /* was [6][21] */, coneta[50],
           eta[50], ata1[36] /* was [6][6] */;
    /* System generated locals */
    integer i__1;
/* identify program to user */
    printf("%s\n\n", "Program E1TIM demonstrates use of TIMSER.");
/* initialize */
    n = 36;
    k = 2;
    l = 2;
    p = .9f;
/* perform time series analysis */
    timser_(y, &n, &k, &l, &p, eta, coneta, a, ata1, ata1at, scrat);
/* output results */
    printf("%s%3li%s%3li%s%3li%s%10.5lf\n",
           "N = ", n, ", K = ", k, ", L = ", l, ", P = ", p);
    printf("%s\n", "  I   Y         ETA       CONETA");
    i__1 = n + k;
    for (i = -k + 1; i <= i__1; ++i) {
	if (i < 1 || i > n) {
            printf("%4li          %10.5lf%10.5lf\n",
                   i, eta[i+k-1], coneta[i+k-1]);
	} else {
            printf("%4li%10.5lf%10.5lf%10.5lf\n",
                   i, y[i-1], eta[i+k-1], coneta[i+k-1]);
	}
    }
    return 0;
} /* main */

