#ifndef _QPROCLIST_
#define _QPROCLIST_

#include "QList.h"
#include "QProcessor.h"

class QProcList: public QProcessor
{
  public:
    QProcList(): QProcessor(), fQPL(new QList<TObject*>), fPProcessor(fDefPProcessor), fPPUsesMain(kTRUE), fThreads(NULL), fNThreads(0), fMutexes(NULL), fIMutexes(NULL), fFMutexes(NULL), fStopThreads(kTRUE), fAIObjects(NULL), fAOObjects(NULL) {}
    QProcList(const char *name, const char *title): QProcessor(name,title), fQPL(new QList<TObject*>), fPProcessor(fDefPProcessor), fPPUsesMain(kTRUE), fThreads(NULL), fNThreads(0), fMutexes(NULL), fIMutexes(NULL), fFMutexes(NULL), fStopThreads(kTRUE), fAIObjects(NULL), fAOObjects(NULL) {}
    QProcList(const QProcList &rhs): QProcessor(rhs), fQPL(new QList<TObject*>(*rhs.fQPL)), fPProcessor(rhs.fPProcessor), fPPUsesMain(rhs.fPPUsesMain), fThreads(NULL), fNThreads(0), fMutexes(NULL), fIMutexes(NULL), fFMutexes(NULL), fStopThreads(kTRUE), fAIObjects(NULL), fAOObjects(NULL) {}

    virtual ~QProcList();

    void AddQProc(QProcessor* const qproc, const Int_t &index=-1){fQPL->Add(qproc,index);}

    void Analyze();

    void DeleteChildren(const Int_t &nsublevels=0);

    void Exec() const;

    static const Bool_t& GetDefPProcessor(){return fDefPProcessor;}

    Int_t GetNQProcs() const{return fQPL->Count();}

    const Bool_t& GetPProcessor() const{return fPProcessor;}
    const Bool_t& GetPPUsesMain() const{return fPPUsesMain;}

    void InitProcess(Bool_t allocateparammem=kTRUE);
    void InitThreads();

    void PrintAnalysisResults() const;
    void PrintProcesses(const UInt_t &level=0, const Bool_t &printdeps=kTRUE) const;

    const QProcList& operator=(const QProcList &rhs);
    QProcessor& operator[](const Int_t &index) const{return *((QProcessor*)(*fQPL)[index]);}

    void RemoveQProc(const Int_t &index=-1){fQPL->Del(index);}

    static void SetDefPProcessor(const Bool_t &pprocessor=kTRUE){fDefPProcessor=pprocessor;}

    void SetForceExecAll(const Bool_t &forceexecall=kFALSE);

    void SetParamAddress(const Int_t &index, Double_t* const paddr=NULL);
    void SetParamAddress(const char *paramname, Double_t* const paddr=NULL){QProcessor::SetParamAddress(paramname,paddr);}

    void SetPProcessor(const Bool_t &pprocessor=kFALSE){fPProcessor=pprocessor;}
    void SetPPUsesMain(const Bool_t &ppusesmain=kTRUE){fPPUsesMain=ppusesmain;}

    void TerminateProcess();
    void TerminateThreads();

    void Browse(TBrowser *b);

    struct SChainConfig {
      QProcessor **Procs;          //List of QProcessors objects. Stored in reverse order
      Int_t       NProcs;          //Number of procs
      pthread_mutex_t **DOMutexes; //List of mutexes on which the current thread depends on. Stored in reverse order
      Int_t       NDOMutexes;      //Number of mutexes in DOMutexes
      pthread_mutex_t **DMutexes;  //List of dependent mutexes. Stored in reverse order
      Int_t       NDMutexes;       //Number of mutexes in DMutexes
    };

    struct SThreadConfig {
      pthread_t   Thread;
      Bool_t      *Stop;
      SChainConfig *Chains;         //List of chains. Stored in reverse order
      Int_t       NChains;          //Number of chains
    };
  protected:
    void BuildObjLists();
    void ClearObjLists();
    const QList<QProcObj*>& GetIObjList() const{return *fAIObjects;}
    const QList<QProcObj*>& GetOObjList() const{return *fAOObjects;}
    QList<TObject*> *fQPL;         //-> List of QProcessor objects
    Bool_t fPProcessor;            //! Use pthreads
    Bool_t fPPUsesMain;             //! Main thread participates to execution
    SThreadConfig *fThreads;       //! List of threads
    Int_t         fNThreads;       //! Number of threads
    QList<void*> *fMutexes; //! List of mutexes used by the threads
    QList<void*> *fIMutexes; //! List of initial mutexes. Stored in reverse order
    QList<void*> *fFMutexes; //! List of final mutexes. Stored in reverse order
    Bool_t fStopThreads;

    static Bool_t fDefPProcessor;
  private:
    QList<QProcObj*>        *fAIObjects; //! All input objects, including input arrays, for all processors (temporary)
    QList<QProcObj*>        *fAOObjects; //! All output objects, including output arrays, for all processors (temporary)
    mutable Int_t	     lIBuf;      //!
    mutable Int_t	     lIBuf2;     //!
    ClassDef(QProcList,1) //Processor of processor objects
};

void* QPLThread(void *args);

extern const QProcList *gQProcList;

#endif
