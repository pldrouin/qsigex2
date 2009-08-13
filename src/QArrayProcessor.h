#ifndef _QARRAYPROCESSOR_
#define _QARRAYPROCESSOR_

#include "QStdProcessor.h"
#include "QProcArray.h"
#include "QProcBranchHandler.h"
#include "QProcQOAHandler.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QArrayProcessor: public QStdProcessor
{
  public:
    QArrayProcessor(): QStdProcessor(), fSelProcs(new QList<Bool_t>), fTrigAEProcs(new QList<Bool_t>), fNAEProcs(0), fIANames(new QList<QList<TString> >), fOANames(new QList<QList<TString> >), fBuNames(new QList<TString>), fIAIndices(new QList<QList<Int_t> >), fIOIndices(new QList<QList<Int_t> >), fOAIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fSelDepProcs(new QList<Bool_t>), fIASProc(new QList<Bool_t>), fOASProc(new QList<Bool_t>), fAPDepends(new QList<QMask>), fObjsPDepends(new QList<QMask>), fIArrays(new QList<QProcArray*>), fIObjects(new QList<QProcObj*>), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>), fBuffers(new QList<void*>) {}
    QArrayProcessor(const char* name, const char* title): QStdProcessor(name,title), fSelProcs(new QList<Bool_t>), fTrigAEProcs(new QList<Bool_t>), fNAEProcs(0), fAnalysisDir(), fIANames(new QList<QList<TString> >), fOANames(new QList<QList<TString> >), fBuNames(new QList<TString>), fIAIndices(new QList<QList<Int_t> >), fIOIndices(new QList<QList<Int_t> >), fOAIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fSelDepProcs(new QList<Bool_t>), fIASProc(new QList<Bool_t>), fOASProc(new QList<Bool_t>), fAPDepends(new QList<QMask>), fObjsPDepends(new QList<QMask>), fIArrays(new QList<QProcArray*>), fIObjects(new QList<QProcObj*>), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>), fBuffers(new QList<void*>), fAIObjects(NULL), fAOObjects(NULL) {}
    QArrayProcessor(const QArrayProcessor &rhs): QStdProcessor(rhs), fSelProcs(new QList<Bool_t>(*rhs.fSelProcs)), fTrigAEProcs(new QList<Bool_t>(*rhs.fTrigAEProcs)), fNAEProcs(0), fAnalysisDir(rhs.fAnalysisDir), fIANames(new QList<QList<TString> >(*rhs.fIANames)), fOANames(new QList<QList<TString> >(*rhs.fOANames)), fBuNames(new QList<TString>(*rhs.fBuNames)), fIAIndices(new QList<QList<Int_t> >(*rhs.fIAIndices)), fIOIndices(new QList<QList<Int_t> >(*rhs.fIOIndices)), fOAIndices(new QList<QList<Int_t> >(*rhs.fOAIndices)), fOOIndices(new QList<QList<Int_t> >(*rhs.fOOIndices)), fSelDepProcs(new QList<Bool_t>(*rhs.fSelDepProcs)), fIASProc(new QList<Bool_t>(*rhs.fIASProc)), fOASProc(new QList<Bool_t>(*rhs.fOASProc)), fAPDepends(new QList<QMask>(*rhs.fAPDepends)), fObjsPDepends(new QList<QMask>(*rhs.fObjsPDepends)), fIArrays(new QList<QProcArray*>), fIObjects(new QList<QProcObj*>(*rhs.fIObjects)), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>(*rhs.fOObjects)), fBuffers(new QList<void*>), fAIObjects(NULL), fAOObjects(NULL) {}
    virtual ~QArrayProcessor();

    Int_t AddProc(const char* name, const char* title=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    Int_t AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    Int_t AddProc(const char *name, const char *title, const char *procname, Bool_t selector=kFALSE, Int_t index=-1);
    Int_t AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    Int_t AddPSProc(const char* name, const char* title=NULL, Bool_t selectedonly=kFALSE, Int_t index=-1);
    Int_t AddPSProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Bool_t selectedonly=kFALSE, Int_t index=-1);
    Int_t AddPSProc(const char *name, const char *title, const char *procname, Bool_t selectedonly=kFALSE, Int_t index=-1);
    Int_t AddPSProc(const char *name, const char *title, void *proc, const char *procname=NULL, Bool_t selectedonly=kFALSE, Int_t index=-1);

    void Analyze();

    void DelProc(Int_t index=-1){if(index==-1) index=fProcs->Count()-1; fProcs->Del(index); fSelProcs->Del(index); if(index < fNAEProcs) fNAEProcs--;}
    void DelProc(const char *procname);

    void Exec() const;

    Int_t GetNPSProcs() const{return fProcs->Count()-fNAEProcs;} //Number of post selection processes

    void InitProcess(Bool_t allocateparammem=kTRUE);

    const QArrayProcessor& operator=(const QArrayProcessor &rhs);

    void PrintAnalysisResults() const;
    void PrintProcesses(const UInt_t &level=0, const Bool_t &printdeps=kTRUE) const;

    void TerminateProcess();

    void Browse(TBrowser *b);

  protected:
    Int_t AEProcIndexToIndex(const Int_t &index);
    Int_t PSProcIndexToIndex(const Int_t &index);
    void BuildObjLists();
    void ClearObjLists(){if(fAIObjects) {delete fAIObjects; fAIObjects=NULL; delete fAOObjects; fAOObjects=NULL;}}
    const QList<QProcObj*>& GetIObjList() const{return *fAIObjects;}
    const QList<QProcObj*>& GetOObjList() const{return *fAOObjects;}
    static Int_t AddUniqueArray(QList<QList<TString> > *arraylist, const QList<TString> &arraydesc, const Int_t &deftype=-1);
    static Int_t FindArray(const QList<QList<TString> > *arraylist, const QList<TString> &arraydesc);
    static Int_t AddUniqueBuffer(QList<TString> *buflist, const Char_t *bufdesc, const Int_t &deftype=-1);
    static Int_t FindBuffer(const QList<TString> *buflist, const Char_t *bufdesc);

  private:
    QList<Bool_t>            *fSelProcs; //-> List of selector processes / of post-selection processes that explicitely process only selected events
    QList<Bool_t>         *fTrigAEProcs; //-> List of processes / of post-selection processes that trigger the selectors since they have, in addition to selected events input arrays,  all events input arrays and/or they are non-post-processing processes with output objects.
    Int_t                     fNAEProcs; //   Number of non-post-selection processes
    TString                fAnalysisDir; //   Directory returned by gDirectory->GetPath() during the last call of Analyze()
    QList<QList<TString> >    *fIANames; //-> Names of arrays that are used as inputs
    QList<QList<TString> >    *fOANames; //-> Names of generated arrays
    QList<TString>            *fBuNames; //-> Names of output buffers
    QList<QList<Int_t> >    *fIAIndices; //-> Indices of arrays that are used as input for each process
    QList<QList<Int_t> >    *fIOIndices; //-> Indices of objects that are used as input for each process
    QList<QList<Int_t> >    *fOAIndices; //-> Indices of arrays that are used as output for each process
    QList<QList<Int_t> >    *fOOIndices; //-> Indices of objects that are used as output for each process
    QList<Bool_t>         *fSelDepProcs; //-> Indicate if a given process depends directly or indirectly on a selector process, or if it is itself a selector process
    QList<Bool_t>             *fIASProc; //-> Direct or indirect dependency of input arrays on a selector process
    QList<Bool_t>             *fOASProc; //-> Direct or indirect dependency of output arrays on a selector process
    QList<QMask>            *fAPDepends; //-> Dependencies of processes on input arrays
    QList<QMask>         *fObjsPDepends; //-> Dependencies of processes on input objects
    QList<QProcArray*>        *fIArrays; //! Input arrays
    QList<QProcObj*>         *fIObjects; //! Input objects 
    QList<QProcArray*>        *fOArrays; //! Output arrays
    QList<QProcObj*>         *fOObjects; //! Output objects 
    QList<void*>              *fBuffers; //! Buffers for output buffers

    QList<QProcObj*>        *fAIObjects; //! All input objects, including input arrays (temporary)
    QList<QProcObj*>        *fAOObjects; //! All output objects, including output arrays (temporary)

    mutable QMask		lExecpardiffs;    //! Modified parameters since the last call
    mutable QMask		lExecdepmods;     //! Required processes due to modified input arrays or input objects
    //mutable Bool_t		lExecrunall;
    //mutable Int_t		lExeci;
    //mutable Int_t		lExecj;
    mutable QList<QProcArray*>  lExeciaarrays;    //! List for needed input all events arrays
    mutable QList<QProcArray*>  lExecisarrays;    //! List for needed input selected events arrays
    mutable QList<QProcArray*>  lExecoarrays;     //! List for output arrays needing update
    mutable QList<QProcObj*>    lExecoobjects;    //! List for output objects needing update
    mutable QList<Bool_t>       lExecoasproc;     //! Indicate if output array needing update are dependent or not on a selector process
    mutable QList<TObject*>     lExecprocs;       //! List of needed processes
    //mutable Int_t               lExecnaeprocs;    //! Number of processes that process all entries in the array
    mutable QList<Bool_t>       lExecselprocs;    //! Indicate if the needed processes is a selector process or not
    mutable QList<Bool_t>       lExecseldepprocs; //! Indicate if the needed processes depends on a selector process or not
    //mutable Bool_t              lExecdoselection;
    //mutable Long64_t            lExecneaea;       //! Number of entries for arrays that should contain all events
    //mutable Long64_t            lExecneaealast;   //! Number of entries for the previous input array that should contain all events
    //mutable Long64_t            lExecnesea;       //! Number of entries for arrays that should contain only selected events
    //mutable Long64_t            lExecnesealast;   //! Number of entries for the previous input array that should contain only selected events
    //mutable Long64_t            lExecli;
    //mutable Bool_t              lExeceventselected;

    mutable QList<Bool_t>	fNeededIA; //Needed input arrays
    mutable QList<Bool_t>	fNeededOA; //Needed output arrays
    mutable QList<Bool_t>	fNeededOO; //Needed output objects

    ClassDef(QArrayProcessor,1) //Arrays and objects processor
};

#include "debugger.h"

#endif
