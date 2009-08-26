#ifndef _QATOMIC_
#define _QATOMIC_

#ifndef __INTEL_COMPILER
#ifdef GCC_VERSION
#undef GCC_VERSION
#endif
#define GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)
#if GCC_VERSION <=40100
#error Atomic operations not implemented in gcc GCC_VERSION
#endif
#endif

#ifdef GCC_VERSION
#define q_load(ptr) _sync_add_and_fetch(ptr,0)
#else
#ifdef __LP64__
template <typename U> U q_load(U const volatile *ptr) {U ret=0; __asm__ __volatile__ ("lock; addq %1, %0" : "=q" (ret) : "m" (*ptr)); return ret;}
#else
template <typename U> U q_load(U const volatile *ptr) {
  U ret=0;
  __asm__ __volatile__ (
      "lock; add %1, %0"
      :"=q" (ret)
      :"m" (*ptr)
      );
  return ret;
}
long long int q_load(long long int const volatile *ptr) {
  long long int ret=0;
  //Need to push one general register to stack
  __asm__ __volatile__ (
      "pushl %%ebx;"
      "0: movl %0, %%eax; movl 4%0, %%edx;"
      "movl %%eax, %%ebx; movl %%edx, %%ecx;"
      "lock; cmpxchg8b %0;"
      "jnz 0b;"
      "movl %%ebx, %1; movl %%ecx, 4%1;"
      "popl %%ebx"
      :"=m" (*ptr), "=m" (ret)
      :"m" (*ptr)
      :"eax", "edx", "ecx", "cc"
      );
  return ret;
}
#endif
#endif

#ifdef GCC_VERSION
#define q_store(ptr,val) _sync_lock_test_and_set(ptr,val)
#else
#define q_store(ptr,val) __asm__ __volatile__ ("lock; xchg %0, %1" : "=m" (*ptr), "=q" (val) : "m" (*ptr), "q" (val))
#endif

#ifdef GCC_VERSION
#define q_add_and_fetch(ptr,val) _sync_add_and_fetch(ptr,val)
#else
template <typename U, typename V> U q_add_and_fetch(U volatile *ptr, const V &val){return q_add_and_fetch(ptr,(U)val);}
template <typename U> U q_add_and_fetch(U volatile *ptr, const U &val) {
  U ret;
  __asm__ __volatile__ (
      "0: mov %0, %4; add %4, %2; lock; cmpxchg %2, %0; jnz 0b"
      :"=m" (*ptr), "=q" (ret)
      :"1" (val), "m" (*ptr), "a" (ret)
      :"cc"
      );
  return ret;
}
/*long long int q_add_and_fetch(long long int volatile *ptr, const long long int &val) {
  long long int ret;
  __asm__ __volatile__ (
  );
  return ret;
}*/
#endif

#endif
