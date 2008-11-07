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
    QArrayProcessor(): QStdProcessor(), fSelProcs(new QList<Bool_t>), fNAEProcs(0), fIANames(new QList<QList<TString> >), fOANames(new QList<QList<TString> >), fBuNames(new QList<TString>), fIAIndices(new QList<QList<Int_t> >), fIOIndices(new QList<QList<Int_t> >), fOAIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fSelDepProcs(new QList<Bool_t>), fIASProc(new QList<Bool_t>), fOASProc(new QList<Bool_t>), fAPDepends(new QList<QMask>), fObjsPDepends(new QList<QMask>), fIArrays(new QList<QProcArray*>), fIObjects(new QList<QProcObj*>), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>), fBuffers(new QList<Double_t>) {}
    QArrayProcessor(const char* name, const char* title): QStdProcessor(name,title), fSelProcs(new QList<Bool_t>), fNAEProcs(0), fAnalysisDir(), fIANames(new QList<QList<TString> >), fOANames(new QList<QList<TString> >), fBuNames(new QList<TString>), fIAIndices(new QList<QList<Int_t> >), fIOIndices(new QList<QList<Int_t> >), fOAIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fSelDepProcs(new QList<Bool_t>), fIASProc(new QList<Bool_t>), fOASProc(new QList<Bool_t>), fAPDepends(new QList<QMask>), fObjsPDepends(new QList<QMask>), fIArrays(new QList<QProcArray*>), fIObjects(new QList<QProcObj*>), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>), fBuffers(new QList<Double_t>) {}
    QArrayProcessor(const QArrayProcessor &rhs): QStdProcessor(rhs), fSelProcs(new QList<Bool_t>(*rhs.fSelProcs)), fNAEProcs(0), fAnalysisDir(rhs.fAnalysisDir), fIANames(new QList<QList<TString> >(*rhs.fIANames)), fOANames(new QList<QList<TString> >(*rhs.fOANames)), fBuNames(new QList<TString>(*rhs.fBuNames)), fIAIndices(new QList<QList<Int_t> >(*rhs.fIAIndices)), fIOIndices(new QList<QList<Int_t> >(*rhs.fIOIndices)), fOAIndices(new QList<QList<Int_t> >(*rhs.fOAIndices)), fOOIndices(new QList<QList<Int_t> >(*rhs.fOOIndices)), fSelDepProcs(new QList<Bool_t>(*rhs.fSelDepProcs)), fIASProc(new QList<Bool_t>(*rhs.fIASProc)), fOASProc(new QList<Bool_t>(*rhs.fOASProc)), fAPDepends(new QList<QMask>(*rhs.fAPDepends)), fObjsPDepends(new QList<QMask>(*rhs.fObjsPDepends)), fIArrays(new QList<QProcArray*>), fIObjects(new QList<QProcObj*>(*rhs.fIObjects)), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>(*rhs.fOObjects)), fBuffers(new QList<Double_t>){}
    virtual ~QArrayProcessor();

    void AddProc(const char* name, const char* title=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    void AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    void AddProc(const char *name, const char *title, const char *procname, Bool_t selector=kFALSE, Int_t index=-1);
    void AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    void AddPSProc(const char* name, const char* title=NULL, Bool_t selectedonly=kFALSE, Int_t index=-1);
    void AddPSProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Bool_t selectedonly=kFALSE, Int_t index=-1);
    void AddPSProc(const char *name, const char *title, const char *procname, Bool_t selectedonly=kFALSE, Int_t index=-1);
    void AddPSProc(const char *name, const char *title, void *proc, const char *procname=NULL, Bool_t selectedonly=kFALSE, Int_t index=-1);

    void Analyze();

    void DelProc(Int_t index=-1){if(index==-1) index=fProcs->Count()-1; fProcs->Del(index); fSelProcs->Del(index); if(index < fNAEProcs) fNAEProcs--;}
    void DelProc(const char *procname);

    void Exec() const;

    Int_t GetNPSProcs() const{return fProcs->Count()-fNAEProcs;} //Number of post selection processes

    void InitProcess(Bool_t allocateparammem=kTRUE);

    const QArrayProcessor& operator=(const QArrayProcessor &rhs);

    void PrintAnalysisResults() const;
    void PrintProcesses(UInt_t level=0) const;

    void TerminateProcess();

    void Browse(TBrowser *b);

  protected:
    Int_t AEProcIndexToIndex(Int_t index);
    Int_t PSProcIndexToIndex(Int_t index);

  private:
    QList<Bool_t>            *fSelProcs; //-> List of selector processes / of post-selection processes that explicitely process only selected events
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
    QList<Bool_t>             *fIASProc; //-> Dependency of input arrays on a selector process
    QList<Bool_t>             *fOASProc; //-> Dependency of output arrays on a selector process
    QList<QMask>            *fAPDepends; //-> Dependencies of processes on input arrays
    QList<QMask>         *fObjsPDepends; //-> Dependencies of processes on input objects
    QList<QProcArray*>        *fIArrays; //! Input arrays
    QList<QProcObj*>         *fIObjects; //! Input objects 
    QList<QProcArray*>        *fOArrays; //! Output arrays
    QList<QProcObj*>         *fOObjects; //! Output objects 
    QList<Double_t>           *fBuffers; //! Buffers for output buffers

    mutable QList<Bool_t>	fNeededIA; //Needed input arrays
    mutable QList<Bool_t>	fNeededOA; //Needed output arrays
    mutable QList<Bool_t>	fNeededOO; //Needed output objects

    ClassDef(QArrayProcessor,1) //Arrays and objects processor
};

#include "debugger.h"

#endif
