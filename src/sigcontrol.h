// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _SIGCONTROL_
#define _SIGCONTROL_

#ifndef __CINT__
#include <signal.h>
#include <pthread.h>
#endif

#define block_signals(pnset, poset, err) if(sigprocmask(SIG_SETMASK,pnset,poset)) {perror("sigprocmask"); err;}
#define unblock_signals(poset,err) if(sigprocmask(SIG_SETMASK,poset,NULL)) {perror("sigprocmask"); err;}

#define block_signals_init(nset,oset,err) sigset_t oset,nset; if(sigfillset(&nset)) {perror("sigfillset"); err;}

#define block_signals_once(err) block_signals_init(nset,oset,err) block_signals(&nset,&oset,err)
#define unblock_signals_once(err) unblock_signals(&oset,err)

#define pthread_block_signals(pnset, poset, err) if(pthread_sigmask(SIG_SETMASK,pnset,poset)) {perror("pthread_sigmask"); err;}
#define pthread_unblock_signals(poset,err) if(pthread_sigmask(SIG_SETMASK,poset,NULL)) {perror("pthread_sigmask"); err;}

#define pthread_block_signals_once(err) block_signals_init(nset,oset,err) pthread_block_signals(&nset,&oset,err)
#define pthread_unblock_signals_once(err) pthread_unblock_signals(&oset,err)

#endif
