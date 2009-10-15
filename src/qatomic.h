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

//#undef GCC_VERSION

#ifdef GCC_VERSION
#define q_load(ptr) __sync_add_and_fetch(ptr,0)
#else
#ifdef __LP64__
template <typename U> U q_load(U const volatile *ptr) {
  U ret=0;
  __asm__ __volatile__ (
      "lock; xadd %0, %1"
      :"=q" (ret)
      :"m" (*ptr),"0" (ret)
      );
  return ret;
}
#else
template <typename U> U q_load(U const volatile *ptr) {
  U ret=0;
  __asm__ __volatile__ (
      "lock; xadd %0, %1"
      :"=q" (ret)
      :"m" (*ptr),"0" (ret)
      );
  return ret;
}

inline long long int q_load(long long int const volatile *ptr) {
  long long int ret=0;
  //Need to push one general register to stack
  __asm__ __volatile__ (
      "pushl %%ebx;"
      "0: movl %1, %%eax; movl 4%1, %%edx;"
      "movl %%eax, %%ebx; movl %%edx, %%ecx;"
      "lock; cmpxchg8b %1;"
      "jnz 0b;"
      "movl %%ebx, %0; movl %%ecx, 4%0;"
      "popl %%ebx"
      :"=m" (ret)
      :"m" (*ptr)
      :"eax", "edx", "ecx", "cc"
      );
  return ret;
}
#endif
#endif

#ifdef GCC_VERSION
#define q_store(ptr,val) __sync_lock_test_and_set(ptr,val)
#else
#define q_store(ptr,val) __asm__ __volatile__ ("lock; xchg %1, %0" : "=m" (*ptr) : "q" (val), "m" (*ptr))
#endif

#ifdef GCC_VERSION
#define q_add(ptr,val) __sync_add_and_fetch(ptr,val)
#else
template <typename U, typename V> void q_add(U volatile *ptr, const V &val) {
  //add with 64-bit operand not defined for IA-32
  __asm__ __volatile__ (
      "lock; add %1, %0"
      :"+m" (*ptr)
      :"q" ((U)val)
      );
}
#endif

#ifdef GCC_VERSION
#define q_add_and_fetch(ptr,val) __sync_add_and_fetch(ptr,val)
#else
template <typename U, typename V> U q_add_and_fetch(U volatile *ptr, const V &val) {
  U ret=0;
  //cmpxchg with 64-bit operand not defined for IA-32
  //Code is twice as slow as gcc built-in functions
  __asm__ __volatile__ (
      "0: mov %0, %2; mov %2, %1; add %3, %1; lock; cmpxchg %1, %0; jnz 0b"
      :"+m" (*ptr), "=q" (ret)
      :"a" (ret), "q" ((U)val)
      :"cc"
      );
  return ret;
}

/*template <typename V> long long int q_add_and_fetch(long long int volatile *ptr, const  V &val) {
  long long int ret=0;
  __asm__ __volatile__ (
      "0: movl %0, %%eax; movl 4%0, %%edx;"
      "movl %%eax, %%ebx; movl %%edx, %%ecx;"
      "lock; cmpxchg8b %0;"
      "jnz 0b;"
      "movl %%ebx, %%eax; movl %%ecx, 4%%eax;"
      :"+m" (*ptr), "=rax" (ret)
      :"q" ((long long int)val)
      :"ebx", "edx", "ecx", "cc"
      );
  return ret;
}*/
#endif

#endif
