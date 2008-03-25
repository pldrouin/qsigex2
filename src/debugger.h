#ifndef UNDEF
#ifdef DEBUG

#include <iostream>
        using namespace std;

#define PRINTF(a) cout << a;
#define PRINTF2(a,b) cout << a << b;
#define PRINTF3(a,b,c) cout << a << b << c;
#define PRINTF4(a,b,c,d) cout << a << b << c << d;
#define PRINTF5(a,b,c,d,e) cout << a << b << c << d << e;
#define PRINTF6(a,b,c,d,e,f) cout << a << b << c << d << e << f;
#define PRINTF7(a,b,c,d,e,f,g) cout << a << b << c << d << e << f << g;
#define PRINTF8(a,b,c,d,e,f,g,h) cout << a << b << c << d << e << f << g << h;
#define PRINTF9(a,b,c,d,e,f,g,h,i) cout << a << b << c << d << e << f << g << h << i;
#define PRINTF10(a,b,c,d,e,f,g,h,i,j) cout << a << b << c << d << e << f << g << h << i << j;
#define PRINTF11(a,b,c,d,e,f,g,h,i,j,k) cout << a << b << c << d << e << f << g << h << i << j << k;
#define PRINTF12(a,b,c,d,e,f,g,h,i,j,k,l) cout << a << b << c << d << e << f << g << h << i << j << k << l;
#define UNDEF

#else
#define PRINTF(a)
#define PRINTF2(a,b)
#define PRINTF3(a,b,c)
#define PRINTF4(a,b,c,d)
#define PRINTF5(a,b,c,d,e)
#define PRINTF6(a,b,c,d,e,f)
#define PRINTF7(a,b,c,d,e,f,g)
#define PRINTF8(a,b,c,d,e,f,g,h)
#define PRINTF9(a,b,c,d,e,f,g,h,i)
#define PRINTF10(a,b,c,d,e,f,g,h,i,j)
#define PRINTF11(a,b,c,d,e,f,g,h,i,j,k)
#define PRINTF12(a,b,c,d,e,f,g,h,i,j,k,l)
#define UNDEF
#endif

#else
#undef DEBUG
#undef DEBUG2
#undef PRINTF
#undef PRINTF2
#undef PRINTF3
#undef PRINTF4
#undef PRINTF5
#undef PRINTF6
#undef PRINTF7
#undef PRINTF8
#undef PRINTF9
#undef PRINTF10
#undef PRINTF11
#undef PRINTF12
#undef UNDEF

#endif











