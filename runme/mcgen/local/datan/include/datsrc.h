#include "dacconf.h"

START_DECLBLOCK

extern void LIBFUNC auxder_(double (CBFUNC P_T *f)(double const P_T *x) ,
                           double const P_T *x, double P_T *dfdx,
                           logical P_T *ok);
extern void LIBFUNC auxdrg_(double P_T *x, double P_T *eta,
                           integer const P_T *mm, integer const P_T *n,
                           integer const P_T *nr, integer const P_T *nred,
                           integer const P_T *list, double P_T *e,
                           logical P_T *ok);
extern void LIBFUNC auxdri_(double (CBFUNC P_T *f)(double const P_T *x,
                           integer const P_T *nr, double const P_T *tt),
                           double P_T *x, double const P_T *tt,
                           integer const P_T *ny, integer const P_T *nr,
                           integer const P_T *nred, integer const P_T *list,
                           double P_T *aa, logical P_T *ok);
extern void LIBFUNC auxgrd_(double P_T *x, double P_T *grd,
                           integer const P_T *n, integer const P_T *nred,
                           integer const P_T *list,
                           double (CBFUNC P_T *func)(double const P_T *x,
                           integer const P_T *n));
extern void LIBFUNC auxhes_(double P_T *x, double P_T *hesse,
                           integer const P_T *n, integer const P_T *nred,
                           integer const P_T *list,
                           double (CBFUNC P_T *func)(double const P_T *x,
                           integer const P_T *n));
extern void LIBFUNC auxzbr_(double P_T *x0, double P_T *x1,
                           double (CBFUNC P_T *funct)(double const P_T *x0,
                           double const P_T *par, integer const P_T *npar1,
                           integer const P_T *npar2), double const P_T *par,
                           integer const P_T *npar1, integer const P_T *npar2);
extern void LIBFUNC auxzfn_(double P_T *x0, double P_T *x1, double P_T *xzero,
                           double (CBFUNC P_T *funct)(double const P_T *x0,
                           double const P_T *par, integer const P_T *npar1,
                           integer const P_T *npar2), double const P_T *par,
                           integer const P_T *npar1, integer const P_T *npar2,
                           double const P_T *epsiln);
/* contains output to default unit */
extern void LIBFUNC avoutp_(double const P_T *x, integer const P_T *ni,
                           integer const P_T *nj, integer const P_T *nk,
                           double const P_T *q, double const P_T *s,
                           double const P_T *f, integer const P_T *ndf,
                           double const P_T *alpha, integer const P_T *ntype);
extern void LIBFUNC avtble_(double const P_T *x, integer const P_T *ni,
                           integer const P_T *nj, integer const P_T *nk,
                           double P_T *xb, double P_T *xbi, double P_T *xbj,
                           double P_T *xbij, double P_T *q, double P_T *s,
                           double P_T *f, integer P_T *ndf, double P_T *a);
extern double LIBFUNC gbetaf_(double const P_T *z, double const P_T *w);
extern double LIBFUNC gbinco_(integer const P_T *n, integer const P_T *k);
extern double LIBFUNC ggamma_(double const P_T *x);
extern double LIBFUNC gincbt_(double const P_T *aa, double const P_T *bb,
                             double const P_T *xx);
extern double LIBFUNC gincgm_(double const P_T *a, double const P_T *x);
extern double LIBFUNC glngam_(double const P_T *x);
extern double LIBFUNC lsq2ex_(double const P_T *x, integer const P_T *nr,
                             double const P_T *t);
extern void LIBFUNC lsqasg_(double P_T *y, double P_T *cy, double P_T *fy,
                           double P_T *f, double P_T *e, integer const P_T *m,
                           integer const P_T *n, integer const P_T *nr,
                           integer const P_T *nred, integer P_T *list,
                           double const P_T *x0, double P_T *cx,
                           double const P_T *r, double const P_T *w,
                           double P_T *dxplus, double P_T *dxmins,
                           double P_T *a2, integer P_T *nstep);
extern void LIBFUNC lsqasm_(double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *nr, double const P_T *t),
                           double const P_T *t, double const P_T *y,
                           double const P_T *deltay, integer const P_T *n,
                           integer const P_T *nr, integer const P_T *nred,
                           integer P_T *list, double const P_T *x0,
                           double P_T *cx, double const P_T *r,
                           double const P_T *w, double P_T *dxplus,
                           double P_T *dxmins, double P_T *a,
                           double P_T *scrat, integer P_T *nstep);
extern void LIBFUNC lsqasn_(double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *nr, double const P_T *t),
                           double const P_T *t, double const P_T *y,
                           double const P_T *deltay, integer const P_T *n,
                           integer const P_T *nr, integer const P_T *nred,
                           integer P_T *list, double const P_T *x0,
                           double P_T *cx, double const P_T *r,
                           double const P_T *w, double P_T *dxplus,
                           double P_T *dxmins, double P_T *a,
                           double P_T *scrat, integer P_T *nstep);
extern void LIBFUNC lsqcog_(integer const P_T *ix, integer const P_T *jx,
                           double const P_T *xi0, double const P_T *xj0,
                           double const P_T *dxi, double const P_T *dxj,
                           integer const P_T *nxi, integer const P_T *nxj,
                           double const P_T *fcont, double P_T *y,
                           double P_T *cy, double P_T *gy, double P_T *f,
                           double P_T *e, integer const P_T *m,
                           integer const P_T *n, integer const P_T *nr,
                           double P_T *x0, double P_T *a2);
extern void LIBFUNC lsqcon_(integer const P_T *ix, integer const P_T *jx,
                           double const P_T *xi0, double const P_T *xj0,
                           double const P_T *dxi, double const P_T *dxj,
                           integer const P_T *nxi, integer const P_T *nxj,
                           double const P_T *fcont, double P_T *x0,
                           double const P_T *t, double const P_T *y,
                           double const P_T *deltay, integer const P_T *n,
                           integer const P_T *nr,
                           double (CBFUNC P_T *userfn)(double const P_T *x0,
                           integer const P_T *nr, double const P_T *t));
extern double LIBFUNC lsqexp_(double const P_T *x, integer const P_T *nr,
                             double const P_T *t);
extern void LIBFUNC lsqgen_(double P_T *y, double P_T *cy, double P_T *fy,
                           double P_T *f, double P_T *e, integer const P_T *m,
                           integer const P_T *n, integer const P_T *nr,
                           integer const P_T *nred, integer P_T *list,
                           double P_T *x, double P_T *cx, double P_T *r,
                           double P_T *a2, integer P_T *nstep);
extern double LIBFUNC lsqgfn_(double const P_T *eta, double const P_T *x,
                             integer const P_T *n, integer const P_T *nr,
                             integer const P_T *k);
extern double LIBFUNC lsqgss_(double const P_T *x, integer const P_T *nr,
                             double const P_T *t);
extern void LIBFUNC lsqlin_(double const P_T *t, double P_T *c,
                           double const P_T *deltay, integer const P_T *n,
                           integer P_T *nr, double P_T *x, double P_T *cx,
                           double P_T *r, double P_T *a, double P_T *scrat,
                           logical P_T *ok);
extern void LIBFUNC lsqmar_(double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *nr, double const P_T *t),
                           double const P_T *t, double const P_T *y,
                           double const P_T *deltay, integer const P_T *n,
                           integer const P_T *nr, integer P_T *nred,
                           integer P_T *list, double P_T *x, double P_T *cx,
                           double P_T *r, double P_T *a, double P_T *scrat,
                           integer P_T *nstep);
extern void LIBFUNC lsqnon_(double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *nr, double const P_T *t),
                           double const P_T *t, double const P_T *y,
                           double const P_T *deltay, integer const P_T *n,
                           integer const P_T *nr, integer P_T *nred,
                           integer P_T *list, double P_T *x, double P_T *cx,
                           double P_T *r, double P_T *a, double P_T *scrat,
                           integer P_T *nstep);
extern double LIBFUNC lsqp2g_(double const P_T *x, integer const P_T *nr,
                             double const P_T *t);
extern void LIBFUNC lsqpol_(double const P_T *t, double const P_T *y,
                           double const P_T *deltay, integer const P_T *n,
                           integer P_T *nr, double P_T *x, double P_T *cx,
                           double P_T *r, double P_T *a, double P_T *scrat,
                           logical P_T *ok);
extern void LIBFUNC minasy_(double (CBFUNC P_T *userfn)(double const P_T *x0,
                           integer const P_T *n),
                           integer const P_T *n, integer const P_T *nred,
                           integer P_T *list, double const P_T *x0,
                           double const P_T *cx, double const P_T *fcont,
                           double P_T *dxplus, double P_T *dxmins,
                           double P_T *xr, integer P_T *nstep);
extern void LIBFUNC mincjg_(double P_T *x0, integer const P_T *n,
                           integer const P_T *nred, integer const P_T *list,
                           double (CBFUNC P_T *userfn)(double const P_T *x0,
                           integer const P_T *n), double P_T *fmin,
                           double const P_T *epsiln, integer P_T *nstep);
extern void LIBFUNC mincmb_(double P_T *a, double P_T *b,
                           double const P_T *epsiln,
                           double (CBFUNC P_T *userfn)(double const P_T *xline,
                           integer const P_T *n), double P_T *xmin,
                           double P_T *fmin, integer P_T *nstep,
                           double const P_T *x0, double const P_T *xdir,
                           integer const P_T *n);
extern void LIBFUNC mincnt_(integer const P_T *ix, integer const P_T *jx,
                           double const P_T *xi0, double const P_T *xj0,
                           double const P_T *dxi, double const P_T *dxj,
                           integer const P_T *nxi, integer const P_T *nxj,
                           double const P_T *fcont, double P_T *x0,
                           integer const P_T *n,
                           double (CBFUNC P_T *userfn)(double const P_T *x0,
                           integer const P_T *n));
extern void LIBFUNC mincov_(double P_T *x0, integer const P_T *n,
                           integer const P_T *nred, integer const P_T *list,
                           double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *n), double const P_T *fact,
                           double P_T *scrat1, double P_T *scrat2,
                           double P_T *cx, logical P_T *ok);
extern void LIBFUNC mindir_(double P_T *x0, double const P_T *dir,
                           integer const P_T *n,
                           double (CBFUNC P_T *userfn)(double const P_T *xline,
                           integer const P_T *n), double P_T *fmin,
                           integer P_T *nstep, double const P_T *epsiln);
extern void LIBFUNC minenc_(double P_T *xa, double P_T *xb, double P_T *xc,
                           double P_T *ya, double P_T *yb, double P_T *yc,
                           double (CBFUNC P_T *userfn)(double const P_T *xline,
                           integer const P_T *n), integer P_T *nstep,
                           double const P_T *x0, double const P_T *xdir,
                           integer const P_T *n);
extern double LIBFUNC minfnd_(double const P_T *a, double const P_T *x0,
                             double const P_T *xdir, integer const P_T *n,
                             double (CBFUNC P_T *userfn)(double const P_T
                             *xline, integer const P_T *n));
extern void LIBFUNC mingld_(double P_T *a, double P_T *b, double P_T *c,
                           double const P_T *epsiln,
                           double (CBFUNC P_T *userfn)(double const P_T *x,
                           double const P_T *x0, double const P_T *xdir,
                           integer const P_T *n), double P_T *xmin,
                           double P_T *ymin, integer P_T *nstep,
                           double const P_T *x0, double const P_T *xdir,
                           integer const P_T *n);
extern double LIBFUNC minglp_(double const P_T *x, integer const P_T *n);
/* comlen cmingh_ 1616 */
extern double LIBFUNC mingls_(double const P_T *x, integer const P_T *n);
/* comlen cmings_ 8004 */
extern double LIBFUNC mingsq_(double const P_T *x, integer const P_T *n);
/* comlen cmingh_ 1616 */
extern void LIBFUNC minmar_(double P_T *x0, integer const P_T *n,
                           integer P_T *nred, integer const P_T *list,
                           double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *n), double P_T *fmin,
                           double const P_T *epsiln, integer P_T *nstep,
                           double P_T *hesse);
extern void LIBFUNC minpow_(double P_T *x0, integer const P_T *n,
                           integer const P_T *nred, integer const P_T *list,
                           double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *n), double P_T *fmin,
                           double const P_T *epsiln, integer P_T *nstep,
                           double P_T *alldir);
extern void LIBFUNC minprb_(double P_T *xmp, double const P_T *xa,
                           double const P_T *ya, double const P_T *xb,
                           double const P_T *yb, double const P_T *xc,
                           double const P_T *yc);
extern void LIBFUNC minqdr_(double P_T *x0, integer const P_T *n,
                           integer P_T *nred, integer const P_T *list,
                           double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *n), double P_T *fmin,
                           double const P_T *epsiln, integer P_T *nstep,
                           double P_T *hesse);
extern void LIBFUNC minsim_(double P_T *x0, integer const P_T *n,
                           integer const P_T *nred, integer const P_T *list,
                           double (CBFUNC P_T *userfn)(double const P_T *x,
                           integer const P_T *n), double P_T *fmin,
                           double const P_T *epsiln, integer P_T *nstep,
                           double P_T *x);
extern void LIBFUNC mtxadd_(double const P_T *a, double const P_T *b,
                           double P_T *r, integer const P_T *m,
                           integer const P_T *n);
extern void LIBFUNC mtxadv_(double const P_T *u, double const P_T *v,
                           double P_T *w, integer const P_T *n);
extern void LIBFUNC mtxchi_(double P_T *a, double P_T *u, integer const P_T *n);
extern void LIBFUNC mtxchl_(double const P_T *a, double P_T *u,
                           integer const P_T *n);
extern void LIBFUNC mtxchm_(double const P_T *u, double const P_T *a,
                           double P_T *r, integer const P_T *m,
                           integer const P_T *n);
extern void LIBFUNC mtxcpv_(double const P_T *u, double P_T *v,
                           integer const P_T *n);
extern void LIBFUNC mtxdec_(double P_T *a, double P_T *b, double P_T *x,
                           double P_T *r, integer const P_T *m, integer P_T *n,
                           double const P_T *frac, logical P_T *ok,
                           double P_T *d, double P_T *u, double P_T *v);
extern void LIBFUNC mtxdot_(double const P_T *u, double const P_T *v,
                           double P_T *s, integer const P_T *n);
extern void LIBFUNC mtxequ_(double P_T *a, double P_T *b, integer const P_T *n,
                           integer const P_T *m);
extern void LIBFUNC mtxgcl_(double const P_T *a, double P_T *c,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *i);
extern void LIBFUNC mtxgrw_(double const P_T *a, double P_T *r,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *i);
extern void LIBFUNC mtxgsm_(double const P_T *a, double P_T *s,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *k, integer const P_T *l,
                           integer const P_T *m1, integer const P_T *n1);
extern void LIBFUNC mtxgsv_(double const P_T *u, double P_T *v,
                           integer const P_T *n, integer const P_T *nred,
                           integer const P_T *list);
extern void LIBFUNC mtxgva_(double P_T *v1, double P_T *v2, double P_T *c,
                           double P_T *s);
extern void LIBFUNC mtxgvd_(double const P_T *v1, double const P_T *v2,
                           double P_T *c, double P_T *s);
extern void LIBFUNC mtxgvt_(double P_T *z1, double P_T *z2,
                           double const P_T *c, double const P_T *s);
extern void LIBFUNC mtxhsd_(double const P_T *v, double P_T *up, double P_T *b,
                           integer const P_T *n, integer const P_T *lp,
                           integer const P_T *l);
extern void LIBFUNC mtxhst_(double const P_T *v, double const P_T *up,
                           double const P_T *b, double P_T *c,
                           integer const P_T *n, integer const P_T *lp,
                           integer const P_T *l);
extern void LIBFUNC mtxlsc_(double P_T *a, double P_T *b, double P_T *e,
                           double const P_T *d, double P_T *x, double P_T *r,
                           double P_T *a2, integer const P_T *m,
                           integer const P_T *n, integer const P_T *l,
                           double const P_T *frac, logical P_T *ok);
extern void LIBFUNC mtxmar_(double P_T *a, double P_T *b,
                           double const P_T *alam, double P_T *x1,
                           double P_T *x2, integer const P_T *m,
                           integer P_T *n, double const P_T *frac,
                           logical P_T *ok);
extern void LIBFUNC mtxsvm_(double const P_T *a, double const P_T *b,
                           double const P_T *d, double const P_T *alam,
                           double P_T *x1, double P_T *x2,
                           integer const P_T *m, integer P_T *n,
                           double const P_T *frac);
extern void LIBFUNC mtxmat_(double const P_T *a, double const P_T *b,
                           double P_T *r, integer const P_T *m,
                           integer const P_T *l, integer const P_T *n);
extern void LIBFUNC mtxmbt_(double const P_T *a, double const P_T *b,
                           double P_T *r, integer const P_T *m,
                           integer const P_T *l, integer const P_T *n);
extern void LIBFUNC mtxmlt_(double const P_T *a, double const P_T *b,
                           double P_T *r, integer const P_T *m,
                           integer const P_T *l, integer const P_T *n);
extern void LIBFUNC mtxmsc_(double const P_T *a, double P_T *r,
                           double const P_T *s, integer const P_T *m,
                           integer const P_T *n);
extern void LIBFUNC mtxmsv_(double const P_T *u, double P_T *v,
                           double const P_T *s, integer const P_T *n);
extern void LIBFUNC mtxnrv_(double const P_T *u, double P_T *s,
                           integer const P_T *n);
extern void LIBFUNC mtxpcl_(double P_T *a, double const P_T *c,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *i);
extern void LIBFUNC mtxprw_(double P_T *a, double const P_T *r,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *i);
extern void LIBFUNC mtxpsm_(double P_T *a, double const P_T *s,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *k, integer const P_T *l,
                           integer const P_T *m1, integer const P_T *n1);
extern void LIBFUNC mtxpsv_(double P_T *u, double const P_T *v,
                           integer const P_T *n, integer const P_T *nred,
                           integer const P_T *list);
extern void LIBFUNC mtxsbv_(double const P_T *u, double const P_T *v,
                           double P_T *w, integer const P_T *n);
extern void LIBFUNC mtxsub_(double const P_T *a, double const P_T *b,
                           double P_T *r, integer const P_T *m,
                           integer const P_T *n);
extern void LIBFUNC mtxsv1_(double P_T *a, double P_T *b, double P_T *d,
                           double P_T *e, integer const P_T *m,
                           integer const P_T *n, integer const P_T *nb);
extern void LIBFUNC mtxsv2_(double P_T *a, double P_T *b, double P_T *d,
                           double P_T *e, integer const P_T *m,
                           integer const P_T *n, integer const P_T *nb,
                           logical P_T *ok);
extern void LIBFUNC mtxs21_(double P_T *a, double P_T *d, double P_T *e,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *nb, integer const P_T *k);
extern void LIBFUNC mtxs22_(double const P_T *b, double P_T *d, double P_T *e,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *nb, integer const P_T *k,
                           integer const P_T *l);
extern void LIBFUNC mtxs23_(double P_T *a, double P_T *b, double P_T *d,
                           double P_T *e, integer const P_T *m,
                           integer const P_T *n, integer const P_T *nb,
                           integer const P_T *k, integer const P_T *l);
extern void LIBFUNC mtxsv3_(double P_T *a, double P_T *b, double P_T *d,
                           integer const P_T *m, integer const P_T *n,
                           integer const P_T *nb);
extern void LIBFUNC mtxsv4_(double const P_T *a, double P_T *b,
                           double const P_T *d, double P_T *x,
                           double P_T *r, integer const P_T *m,
                           integer P_T *n, integer const P_T *nb,
                           double const P_T *frac);
extern void LIBFUNC mtxsvd_(double P_T *a, double P_T *b, double P_T *x,
                           double P_T *r, integer const P_T *m,
                           integer P_T *n, integer const P_T *nb,
                           double const P_T *frac, logical P_T *ok);
extern void LIBFUNC mtxtra_(double const P_T *a, double P_T *r,
                           integer const P_T *m, integer const P_T *n);
extern void LIBFUNC mtxtrp_(double const P_T *a, double P_T *r,
                           integer const P_T *m, integer const P_T *n);
extern void LIBFUNC mtxunt_(double P_T *r, integer const P_T *n);
/* contains output to default unit */
extern void LIBFUNC mtxwrt_(double const P_T *a, integer const P_T *m,
                           integer const P_T *n);
extern void LIBFUNC mtxzer_(double P_T *r, integer const P_T *n,
                           integer const P_T *m);
extern void LIBFUNC mtxzrv_(double P_T *u, integer const P_T *n);
extern void LIBFUNC regcon_(double const P_T *x, double const P_T *b,
                           double const P_T *t, double const P_T *chi2,
                           double const P_T *p, integer const P_T *nr,
                           integer const P_T *nf, double P_T *eta,
                           double P_T *coneta);
extern void LIBFUNC regpol_(double const P_T *t, double const P_T *y,
                           double const P_T *deltay, integer const P_T *n,
                           integer const P_T *nr, double P_T *x,
                           double P_T *b, double P_T *a, double P_T *chi2);
extern void LIBFUNC rnecuy_(double P_T *u, integer const P_T *n);
extern void LIBFUNC rne2in_(integer const P_T *iseed1,
                           integer const P_T *iseed2);
extern void LIBFUNC rne2ot_(integer P_T *iseed1, integer P_T *iseed2);
extern void LIBFUNC rnline_(double const P_T *a, double const P_T *b,
                           double const P_T *t0, double const P_T *dt,
                           integer const P_T *n, double const P_T *sigmay,
                           double P_T *t, double P_T *y);
extern void LIBFUNC rnmlcg_(double P_T *u, integer const P_T *n);
extern void LIBFUNC rnmsin_(integer const P_T *iseed);
extern void LIBFUNC rnmsot_(integer P_T *iseed);
extern void LIBFUNC rnmngn_(double const P_T *dplus, double const P_T *a,
                           double P_T *x, integer P_T const *n);
extern void LIBFUNC rnmnpr_(double P_T *c, double P_T *dplus, integer P_T *n);
extern void LIBFUNC rnradi_(double const P_T *a, double const P_T *tau1,
                           double const P_T *tau2, double P_T *t);
extern void LIBFUNC rnstnr_(double P_T *r, integer const P_T *n);
extern double LIBFUNC scbinm_(integer const P_T *k, integer const P_T *n,
                             double const P_T *p);
extern double LIBFUNC scchi2_(double const P_T *x, integer const P_T *n);
extern double LIBFUNC scftst_(double const P_T *x, integer const P_T *nf1,
                             integer const P_T *nf2);
extern double LIBFUNC schypg_(integer const P_T *k, integer const P_T *n,
                             integer const P_T *kk, integer const P_T *nn);
extern double LIBFUNC scnorm_(double const P_T *x, double const P_T *x0,
                             double const P_T *sigma);
extern double LIBFUNC scpois_(integer const P_T *k, double const P_T *alambd);
extern double LIBFUNC scstnr_(double const P_T *x);
extern double LIBFUNC scstud_(double const P_T *x, integer const P_T *n);
extern double LIBFUNC sdbinm_(integer const P_T *k, integer const P_T *n,
                             double const P_T *p);
extern double LIBFUNC sdchi2_(double const P_T *x, integer const P_T *n);
extern double LIBFUNC sdftst_(double const P_T *x, integer const P_T *nf1,
                             integer const P_T *nf2);
extern double LIBFUNC sdhypg_(integer const P_T *k, integer const P_T *n,
                             integer const P_T *kk, integer const P_T *nn);
extern double LIBFUNC sdnorm_(double const P_T *x, double const P_T *x0,
                             double const P_T *sigma);
extern double LIBFUNC sdpois_(integer const P_T *k, double const P_T *alambd);
extern double LIBFUNC sdstnr_(double const P_T *x);
extern double LIBFUNC sdstud_(double const P_T *x, integer const P_T *n);
extern void LIBFUNC smerqs_(integer const P_T *k, integer const P_T *id,
                           double const P_T *confid, double const P_T *rback,
                           double P_T *rsmin, double P_T *rspls,
                           double P_T *rsupr);
extern double LIBFUNC smzrqs_(double const P_T *rs, double const P_T *rb,
                             integer const P_T *k, integer const P_T *id);
/* comlen cmsmer_ 8 */
extern void LIBFUNC smerss_(integer const P_T *k, double const P_T *confid,
                           double const P_T *alback, double P_T *alsmin,
                           double P_T *alspls, double P_T *alsupr);
extern double LIBFUNC smzrss_(double const P_T *als, double const P_T *alback,
                             integer const P_T *k, integer const P_T *ndum);
/* comlen cmsmer_ 16 */
extern void LIBFUNC smhsfl_(double P_T *hist, double const P_T *x0,
                           double const P_T *delx, integer const P_T *nx,
                           double const P_T *xin, double const P_T *weight);
extern void LIBFUNC smhsgr_(double const P_T *hist, double const P_T *x0,
                           double const P_T *delx, integer const P_T *nx,
                           char const P_T *tx, integer const P_T *ltx,
                           char const P_T *ty, integer const P_T *lty,
                           char const P_T *capt, integer const P_T *lcapt,
                           integer const P_T *nws);
extern void LIBFUNC smhsin_(double P_T *hist, double const P_T *x0,
                           double const P_T *delx, integer const P_T *nx);
extern void LIBFUNC smmnvr_(double const P_T *data, integer const P_T *n,
                           double P_T *xmean, double P_T *delxm,
                           double P_T *s2, double P_T *dels2, double P_T *s,
                           double P_T *dels);
extern void LIBFUNC smsdgr_(double const P_T *x, double const P_T *y,
                           integer const P_T *n, double const P_T *xa,
                           double const P_T *xb, double const P_T *ya,
                           double const P_T *yb, char const P_T *tx,
                           integer const P_T *ltx, char const P_T *ty,
                           integer const P_T *lty, char const P_T *capt,
                           integer const P_T *lcapt, integer const P_T *nws);
extern double LIBFUNC sqchi2_(double const P_T *p, integer const P_T *n);
extern double LIBFUNC szchi2_(double const P_T *x, double const P_T *p,
                             integer const P_T *n, integer const P_T *ndum);
extern double LIBFUNC sqftst_(double const P_T *p, integer const P_T *n1,
                             integer const P_T *n2);
extern double LIBFUNC szftst_(double const P_T *x, double const P_T *p,
                             integer const P_T *n1, integer const P_T *n2);
extern double LIBFUNC sqnorm_(double const P_T *p, double const P_T *x0,
                             double const P_T *sigma);
extern double LIBFUNC sqpois_(integer const P_T *k, double const P_T *p);
extern double LIBFUNC szpois_(double const P_T *alambd, double const P_T *p,
                             integer const P_T *k, integer const P_T *ndum2);
extern double LIBFUNC sqstnr_(double const P_T *p);
extern double LIBFUNC szstnr_(double const P_T *x, double const P_T *p,
                             integer const P_T *ndum1,
                             integer const P_T *ndum2);
extern double LIBFUNC sqstud_(double const P_T *p, integer const P_T *n);
extern double LIBFUNC szstud_(double const P_T *x, double const P_T *p,
                             integer const P_T *n, integer const P_T *ndum);
extern void LIBFUNC timser_(double const P_T *y, integer const P_T *n,
                           integer const P_T *k, integer const P_T *l,
                           double const P_T *p, double P_T *eta,
                           double P_T *coneta, double P_T *a,
                           double P_T *ata1, double P_T *ata1at,
                           double P_T *scrat);
/*
extern void CALLSEQ P_T grpxct__(double const P_T *x1, double const P_T *x2,
                               double const P_T *y1, double const P_T *y2,
                               double const P_T *f11, double const P_T *f12,
                               double const P_T *f21, double const P_T *f22,
                               double const P_T *fcont);
*/

END_DECLBLOCK
