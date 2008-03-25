#ifndef _DATANDLL
#define _DATANDLL

START_DECLBLOCK
  extern void (LIBFUNC wnwrst)(char const *s, integer const *len);
  extern void (LIBFUNC wnrdst)(char *s, integer const *len);
  extern void (LIBFUNC wninit)(integer *itest);
  extern void (LIBFUNC wnrgpr)(void (CBFUNC *f)());
  extern int (LIBFUNC wnrunp)(void);
END_DECLBLOCK

#endif
