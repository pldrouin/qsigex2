// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QPROCLIST_
#define _QPROCLIST_

#ifndef __CINT__
#include <pthread.h>
#include <semaphore.h>
#include <errno.h>
#else
struct pthread_t;
struct pthread_mutex_t;
struct sem_t;
#endif
#include "sigcontrol.h"
#include "QList.h"
#include "QProcessor.h"

class QProcList: public QProcessor
{
  public:
    QProcList(): QProcessor(), fQPL(new QList<TObject*>), fPProcessor(fDefPProcessor), fChains(NULL), fNChains(0), fIFirstChain(NULL), fILastChain(NULL), fINChains(0), fFirstChain(NULL), fLastChain(NULL), fChMutex(), fTWSem(), fRNotify(NULL), fNRNotify(0), fMTRNotify(NULL), fThreads(NULL), fNThreads(0), fRNThreads(2), fStopThreads(kTRUE), fAIObjects(NULL), fAOObjects(NULL) {pthread_mutex_init(&fChMutex,NULL); sem_init(&fTWSem,0,0);}
    QProcList(const char *name, const char *title): QProcessor(name,title), fQPL(new QList<TObject*>), fPProcessor(fDefPProcessor), fChains(NULL), fNChains(0), fIFirstChain(NULL), fILastChain(NULL), fINChains(0), fFirstChain(NULL), fLastChain(NULL), fChMutex(), fTWSem(), fRNotify(NULL), fNRNotify(0), fMTRNotify(NULL), fThreads(NULL), fNThreads(0), fRNThreads(2), fStopThreads(kTRUE), fAIObjects(NULL), fAOObjects(NULL) {pthread_mutex_init(&fChMutex,NULL); sem_init(&fTWSem,0,0);}
    QProcList(const QProcList &rhs): QProcessor(rhs), fQPL(new QList<TObject*>(*rhs.fQPL)), fPProcessor(rhs.fPProcessor), fChains(NULL), fNChains(0), fIFirstChain(NULL), fILastChain(NULL), fINChains(0), fFirstChain(NULL), fLastChain(NULL), fChMutex(), fTWSem(), fRNotify(NULL), fNRNotify(0), fMTRNotify(NULL), fThreads(NULL), fNThreads(0), fRNThreads(2), fStopThreads(kTRUE), fAIObjects(NULL), fAOObjects(NULL) {pthread_mutex_init(&fChMutex,NULL); sem_init(&fTWSem,0,0);}

    virtual ~QProcList();

    void AddQProc(QProcessor* const qproc, const Int_t &index=-1){block_signals_once(throw 1); fQPL->Add(qproc,index); unblock_signals_once(throw 1);}

    void Analyze();

    void DeleteChildren(const Int_t &nsublevels=0);

    void Exec() const;

    static const Bool_t& GetDefPProcessor(){return fDefPProcessor;}

    const Int_t& GetNThreads() const{return fNThreads;}

    Int_t GetNQProcs() const{return fQPL->Count();}

    const Bool_t& GetPProcessor() const{return fPProcessor;}

    void InitProcess(Bool_t allocateparammem=kTRUE);
    void UpdateProcessFlags();

    void InitThreads();

    void KillThreads(); //For signal-handling only! Does not clear any memory!

    void PrintAnalysisResults() const;
    void PrintProcesses(const UInt_t &level=0, const Bool_t &printdeps=kTRUE) const;

    const QProcList& operator=(const QProcList &rhs);
    QProcessor& operator[](const Int_t &index) const{return *((QProcessor*)(*fQPL)[index]);}

    void RemoveQProc(const Int_t &index=-1){block_signals_once(throw 1); fQPL->Del(index); unblock_signals_once(throw 1);}

    static void SetDefPProcessor(const Bool_t &pprocessor=kTRUE){fDefPProcessor=pprocessor;}

    void SetForceExecAll(const Bool_t &forceexecall=kFALSE);

    void SetNThreads(const Int_t &nthreads){fRNThreads=nthreads;}

    void SetParamActive(const Int_t &index, const Bool_t &active=kTRUE);
    void SetParamActive(const char *paramname, const Bool_t &active=kTRUE){QProcessor::SetParamActive(paramname,active);}

    void SetParamAddress(const Int_t &index, Double_t* const paddr=NULL);
    void SetParamAddress(const char *paramname, Double_t* const paddr=NULL){QProcessor::SetParamAddress(paramname,paddr);}

    void SetPProcessor(const Bool_t &pprocessor=kFALSE){fPProcessor=pprocessor;}

    void TerminateProcess();
    void TerminateThreads();

    void Browse(TBrowser *b);

    struct SRNotify {
      pthread_mutex_t RNCMutex; //Condition mutex for results notification
      Int_t Init;               //Initial readiness counter value
      Int_t RCountDown;         //Result readiness
      sem_t  RNSem;             //Condition for results notification
    };

    struct SChainConfig {
      QProcessor **Procs;          //List of QProcessors objects. Stored in reverse order
      Int_t       NProcs;          //Number of procs
      Int_t	  NDOChains;       //Number of chains on which the chain depends on
      Int_t       NRDOChains;      //Number of remaining DO chains
      pthread_mutex_t DOCMutex;    //Mutex for the number of remaining DO chains
      SChainConfig **DChains;      //List of dependent chains. Stored in reverse order
      Int_t       NDChains;        //Number of dependency chains
      SRNotify   *RNotify;         //Result notification
      SChainConfig *NextInit;      //Next chain that should be queueed for initial chains
      SChainConfig *Next;          //Next chain in queue
    };

  protected:
    void BuildObjLists();
    void ClearObjLists();
    const QList<QProcObj*>& GetIObjList() const{return *fAIObjects;}
    const QList<QProcObj*>& GetOObjList() const{return *fAOObjects;}
    QList<TObject*> *fQPL;         //-> List of QProcessor objects
    Bool_t fPProcessor;            //! Use pthreads
    SChainConfig *fChains;         //! List of chains
    Int_t         fNChains;        //! Number of chains
    SChainConfig *fIFirstChain;    //! Initial first chain pointer
    SChainConfig *fILastChain;     //! Initial last chain pointer
    Int_t fINChains;               //! Number of initial chains
    mutable SChainConfig *fFirstChain;     //! First chain
    mutable SChainConfig *fLastChain;      //! Last chain
    mutable pthread_mutex_t fChMutex;      //! Chain queue mutex
    mutable sem_t fTWSem;          //! Semaphore for thread waiting
    SRNotify        *fRNotify;     //! List of result notifications
    Int_t           fNRNotify;     //! Number of result notifications
    SRNotify      *fMTRNotify;     //! Result notification for the main thread

    pthread_t       *fThreads;     //! List of threads
    Int_t           fNThreads;     //! Number of threads
    Int_t           fRNThreads;    //! Number of requested threads
    Bool_t	  fStopThreads;    //!

    friend void* QPLThread(void *args);
    static Bool_t fDefPProcessor;
  private:
    QList<QProcObj*>        *fAIObjects; //! All input objects, including input arrays, for all processors (temporary)
    QList<QProcObj*>        *fAOObjects; //! All output objects, including output arrays, for all processors (temporary)
    ClassDef(QProcList,1) //Processor of processor objects
};

void* QPLThread(void *args);

extern const QProcList *gQProcList;

#endif
