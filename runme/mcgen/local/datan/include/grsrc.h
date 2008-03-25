#ifndef _DAGRINC_
#define _DAGRINC_

#include "dacconf.h"

#if defined(WINDOWS)
#  define GRFUNC DLLFUNC
#else
#  define GRFUNC LIBFUNC
#endif

START_DECLBLOCK

 extern void (GRFUNC grplin_)(integer const P_T *n, double const P_T *xpl,
                    double const P_T *ypl);
 extern void (GRFUNC gropen_)(void);
 extern void (GRFUNC grclse_)(void);
 extern void (GRFUNC grboun_)(void);
 extern void (GRFUNC grfram_)(void);
 extern void (GRFUNC grmark_)(integer const P_T *nmark, double const P_T *scalef,
                    double const P_T *xpm, double const P_T *ypm);
 extern void (GRFUNC grdatp_)(integer const P_T *nmark, double const P_T *scalef,
                    double const P_T *x, double const P_T *y,
                    double const P_T *sigmax, double const P_T *sigmay,
                    double const P_T *rho);
 extern void (GRFUNC grbrpl_)(integer const P_T *ntype, double const P_T *scd,
                    integer const P_T *n, double const P_T *xpld,
                    double const P_T *ypld);
 extern void (GRFUNC grccrs_)(void);
 extern void (GRFUNC grdtcv_)(double const P_T *xpl, double const P_T *ypl,
                    integer const P_T *npl, integer const P_T *nmark,
                    double const P_T *scalef, double const P_T *datx,
                    double const P_T *daty, double const P_T *datsx,
                    double const P_T *datsy, double const P_T *datcov,
                    integer const P_T *ndat, char const P_T *tx,
                    integer const P_T *ltx, char const P_T *ty,
                    integer const P_T *lty, char const P_T *capt,
                    integer const P_T *lcapt, integer const P_T *nws);
extern void (GRFUNC grdtmc_)(double const P_T *xpl, double const P_T *ypl,
                    integer const P_T *npl, integer const P_T *ncurve,
                    integer const P_T *ncol, integer const P_T *nmark,
                    double const P_T *scalef, double const P_T *datx,
                    double const P_T *daty, double const P_T *datsx,
                    double const P_T *datsy, double const P_T *datcov,
                    integer const P_T *ndat, char const P_T *tx,
                    integer const P_T *ltx, char const P_T *ty,
                    integer const P_T *lty, char const P_T *capt,
                    integer const P_T *lcapt, integer const P_T *nws);
 extern void (GRFUNC grhscv_)(double const P_T *xpl, double const P_T *ypl,
                    integer const P_T *npl, double const P_T *hist,
                    double const P_T *x0, double const P_T *delx,
                    integer const P_T *nx, char const P_T *tx,
                    integer const P_T *ltx, char const P_T *ty,
                    integer const P_T *lty, char const P_T *capt,
                    integer const P_T *lcapt, integer const P_T *nws);
 extern void (GRFUNC grhsdr_)(double const P_T *hist, double const P_T *x0,
                    double const P_T *delx, integer const P_T *nx);
 extern void (GRFUNC grplct_)(double const P_T *x0, double const P_T *y0,
                    double const P_T *dx, double const P_T *dy,
                    integer const P_T *nx, integer const P_T *ny,
                    double const P_T *cont, double (CBFUNC P_T *userfn)
                    (double const P_T *x1, double const P_T *x2));
 extern void (GRFUNC grpxct_)(double const P_T *x1, double const P_T *x2,
                    double const P_T *y1, double const P_T *y2,
                    double const P_T *f11, double const P_T *f12,
                    double const P_T *f21, double const P_T *f22,
                    double const P_T *fcont);
 extern void (GRFUNC grsclx_)(char const P_T *string, integer const P_T *len);
 extern void (GRFUNC grscly_)(char const P_T *string, integer const P_T *len);
 extern void (GRFUNC grscdf_)(double const P_T *fix, double const P_T *del,
                    integer const P_T *ntic, double const P_T *ticlen,
                    double const P_T *grscal, logical const P_T *lnum);
 extern void (GRFUNC grstfr_)(integer const P_T *nws, double const P_T *wd,
                    double const P_T *hd);
 extern void (GRFUNC grtxtc_)(double const P_T *scalef, char const P_T *string,
                    integer const P_T *len);
 extern void (GRFUNC grtxtf_)(double const P_T *x, double const P_T *y,
                    double const P_T *scalef, char const P_T *string,
                    integer const P_T *len);
 extern void (GRFUNC grwncc_)(double const P_T *xacc, double const P_T *xbcc,
                    double const P_T *yacc, double const P_T *ybcc);
 extern void (GRFUNC grwnwc_)(double const P_T *xapwc, double const P_T *xbpwc,
                    double const P_T *yapwc, double const P_T *ybpwc);
 extern void (GRFUNC grvwwc_)(double const P_T *xawc, double const P_T *xbwc,
                    double const P_T *yawc, double const P_T *ybwc);
 extern void (GRFUNC gropws_)(integer const P_T *nws);
 extern void (GRFUNC grclws_)(integer const P_T *nws);
 extern void (GRFUNC grstcl_)(integer const P_T *icol);
 extern void (GRFUNC grnbws_)(void);

 END_DECLBLOCK

#endif
