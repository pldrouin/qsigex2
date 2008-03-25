#ifndef _DATANACCONF_
#define _DATANACCONF_

typedef long int integer;
typedef long int logical;

#ifdef _MSC_VER
#  ifdef __cplusplus
#    define START_DECLBLOCK extern "C"{
#    define END_DECLBLOCK };
#    define CFUNC __cdecl
#  else
#    define START_DECLBLOCK
#    define END_DECLBLOCK
#    define CFUNC
#  endif
#  include <limits.h>
#  if defined(_WIN32)
#    if !defined(WINDOWS)
#      define WINDOWS 32
#    endif
#    include <windows.h>
#    include <windowsx.h>
#    define MEMALLOC(nbytes) GlobalAllocPtr(GMEM_MOVEABLE,nbytes)
#    define MEMFREE(ptr) GlobalFreePtr(ptr)
#    define MAXALLOC ULONG_MAX
#    define MEMERROR(subr)
#    if defined(USEHUGE)
#      undef USEHUGE
#    endif
#    define F_P(name) name
#    define CALLSEQ __stdcall
#    define CBFUNC __stdcall
#    define P_T
#  else
#    define MEMALLOC(nbytes) malloc((size_t)(nbytes))
#    define MEMFREE(ptr) free(ptr)
#    define MAXALLOC UINT_MAX
#    if defined(DEBUG)
#      include <stdio.h>
#      define MEMERROR(subr) printf("%s\n", subr);
#    else
#      define MEMERROR(subr)
#    endif
#    if defined(USEHUGE)
#      define F_P(name) _f_ ## name
#      define HFP(type,name) type __huge *name = (type __huge *)F_P(name)
#    else
#      define F_P(name) name
#    endif
#    if _MSC_VER <= 800
#      define CALLSEQ __fortran
#      define CBFUNC __fortran
#      define P_T far
#    else
#      define CALLSEQ
#      define CBFUNC
#      define P_T
#    endif
#  endif
#  define EXPORT
#elif defined __TURBOC__
#  ifdef __cplusplus
#    define START_DECLBLOCK extern "C"{
#    define END_DECLBLOCK };
#    define CFUNC __cdecl
#  else
#    define START_DECLBLOCK
#    define END_DECLBLOCK
#    define CFUNC
#  endif
#  include <limits.h>
#  ifdef __WIN32__
#    if !defined(WINDOWS)
#      define WINDOWS 32
#    endif
#    include <windows.h>
#    include <win32/winbase.h>
#    include <win32/windowsx.h>
#    define MEMALLOC(nbytes) GlobalAllocPtr(GMEM_MOVEABLE,nbytes)
#    define MEMFREE(ptr) GlobalFreePtr(ptr)
#    define MAXALLOC ULONG_MAX
#    define MEMERROR(subr)
#    if defined(USEHUGE)
#      undef USEHUGE
#    endif
#    define F_P(name) name
#    define CALLSEQ __stdcall
#    define CBFUNC __stdcall
#    define P_T
#  else
#    include <alloc.h>
#    define MEMALLOC(nbytes) farmalloc(nbytes)
#    define MEMFREE(ptr) farfree(ptr)
#    define MAXALLOC ULONG_MAX
#    define MEMERROR(subr)
#    if defined(USEHUGE)
#      define F_P(name) _f_ ## name
#      define HFP(type,name) type __huge *name = (type __huge *)F_P(name)
#    else
#      define F_P(name) name
#    endif
#    define CALLSEQ __pascal
#    define CBFUNC __pascal
#    if defined(__LARGE__)
#      define P_T far
#    else
#      define P_T
#    endif
#  endif
#  define EXPORT
#else
#  ifdef __cplusplus
#    define START_DECLBLOCK extern "C"{
#    define END_DECLBLOCK };
#    define CFUNC __cdecl
#  else
#    define START_DECLBLOCK
#    define END_DECLBLOCK
#    define CFUNC
#  endif
#  include <stdio.h>
#  include <limits.h>
#  define MEMALLOC(nbytes) malloc(nbytes)
#  define MEMFREE(ptr) free(ptr)
#  define MAXALLOC UINT_MAX
#  if defined(DEBUG)
#    define MEMERROR(subr) \
     fprintf(stderr,"Memory allocation error in %s\n.",subr)
#  else
#    define MEMERROR(subr)
#  endif
#  define F_P(name) name
#  define CALLSEQ
#  define CBFUNC
#  define P_T
#  define EXPORT
#endif

#define LIBFUNC EXPORT CALLSEQ P_T
#define LOCLIBFUNC CALLSEQ P_T
#define LIBDATA EXPORT CALLSEQ P_T

#if defined(WINDOWS)
#  include "datandll.h"
#  define MAINCALL(prog) \
   int WINAPI WinMain(HINSTANCE h_Inst, HINSTANCE h_Prev, \
                      LPSTR lp_Cmd, int n_Cmd) \
   { \
     integer result; \
     wninit(&result); \
     if(result) return -1; \
     wnrgpr(prog); \
     wnrunp(); \
     return 0; \
   }
#endif

/*#if defined(WINDOWS)
#  define MEMERROR(subr)
#else
#  define MEMERROR(subr) \
        fprintf(stderr, "Memory allocation error in %s\n.", subr)
#endif
#if defined(_MSC_VER)*/

#ifdef abs
#undef abs
#endif
#ifdef dabs
#undef dabs
#endif
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif
#define abs(x) ((x) >= 0 ? (x) : -(x))
#define dabs(x) (double)abs(x)
#define min(a,b) ((a) <= (b) ? (a) : (b))
#define max(a,b) ((a) >= (b) ? (a) : (b))
#define anint(a) ((a) >= 0. ? floor((a) + .5) : (-floor(.5 - (a))))
#define nint(a) ((a) >= 0. ? (integer)floor((a) + .5) :\
                             (integer)(-floor(.5 - (a))))
#define TRUE_ (1)
#define FALSE_ (0)

#endif
