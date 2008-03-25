#include "datsrc.h"

void LIBFUNC auxhes_(double P_T *x, double P_T *hesse, integer const P_T *n,
             integer const P_T *nred, integer const P_T *list,
             double (CBFUNC P_T *func)(double const P_T *,
             integer const P_T *))
{
    /* System generated locals */
    integer hesse_dim1, hesse_offset, i__1, i__2;
    double d__1;

    /* Local variables */
    integer i, k, il, kl;
    double deli, delk, savi, savk, e, f, fm, fp, dfdxim, dfdxip;

    /* Parameter adjustments */
    --x;
    hesse_dim1 = *nred;
    hesse_offset = hesse_dim1 + 1;
    hesse -= hesse_offset;
    --list;

    /* Function Body */
    if (*nred > 0) {
        il = 0;
        i__1 = *n;
        for (i = 1; i <= i__1; ++i) {
            kl = 0;
            if (list[i] == 1 || *n == *nred) {
                ++il;
                i__2 = *n;
                for (k = 1; k <= i__2; ++k) {
                    if (list[k] == 1 || *nred == *n) {
                        ++kl;
                        if (kl < il) {
                            hesse[il + kl * hesse_dim1] = hesse[kl + il * 
                                    hesse_dim1];
                        } else {
                            e = (d__1 = x[i], abs(d__1));
                            if (e < 1e-9) {
                                e = 1e-9;
                            }
                            deli = e * 1e-5;
                            savi = x[i];
                            if (k == i) {
                                f = (*func)(&x[1], n);
                                x[i] += deli;
                                fp = (*func)(&x[1], n);
                                x[i] = savi - deli;
                                fm = (*func)(&x[1], n);
                                x[i] = savi;
                                hesse[il + il * hesse_dim1] = ((fp - f) / 
                                        deli - (f - fm) / deli) / deli;
                            } else {
                                e = (d__1 = x[k], abs(d__1));
                                if (e < 1e-9) {
                                    e = 1e-9;
                                }
                                delk = e * 1e-5;
                                savk = x[k];
                                x[k] = savk + delk;
                                x[i] = savi + deli;
                                fp = (*func)(&x[1], n);
                                x[i] = savi - deli;
                                fm = (*func)(&x[1], n);
                                dfdxip = (fp - fm) / (deli + deli);
                                x[k] = savk - delk;
                                x[i] = savi + deli;
                                fp = (*func)(&x[1], n);
                                x[i] = savi - deli;
                                fm = (*func)(&x[1], n);
                                x[i] = savi;
                                x[k] = savk;
                                dfdxim = (fp - fm) / (deli + deli);
                                hesse[il + kl * hesse_dim1] = (dfdxip - 
                                        dfdxim) / (delk + delk);
                            }
                        }
                    }
                }
            }
        }
    }
    return;
} /* auxhes_ */

