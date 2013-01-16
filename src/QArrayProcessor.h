// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QARRAYPROCESSOR_
#define _QARRAYPROCESSOR_

#include "strdiffer.h"
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
    QArrayProcessor(): QStdProcessor(), fSelProcs(new QList<Bool_t>), fTrigAEProcs(new QList<Bool_t>), fNAEProcs(0), fIANames(new QList<QList<TString> >), fAIANames(new QList<QList<TString> >), fOANames(new QList<QList<TString> >), fBuNames(new QList<TString>), fActiveAIA(NULL), fLastActiveAIA(NULL), fIAIndices(new QList<QList<Int_t> >), fIOIndices(new QList<QList<Int_t> >), fOAIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fSelDepProcs(new QList<Bool_t>), fIASProc(new QList<Bool_t>), fOASProc(new QList<Bool_t>), fAPDepends(new QList<QMask>), fAAPDepends(fAPDepends), fObjsPDepends(new QList<QMask>), fAObjsPDepends(fObjsPDepends), fIArrays(new QList<QProcArray*>), fAIArrays(new QList<QProcArray*>), fAAIArrays(fAIArrays), fIObjects(new QList<QProcObj*>), fAIObjects(new QList<QProcObj*>), fAAIObjects(fAIObjects), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>), fBuffers(new QList<void*>), fActiveAIO(NULL), fLastActiveAIO(NULL) {}
    QArrayProcessor(const char* name, const char* title): QStdProcessor(name,title), fSelProcs(new QList<Bool_t>), fTrigAEProcs(new QList<Bool_t>), fNAEProcs(0), fAnalysisDir(), fIANames(new QList<QList<TString> >), fAIANames(new QList<QList<TString> >), fOANames(new QList<QList<TString> >), fBuNames(new QList<TString>), fActiveAIA(NULL), fLastActiveAIA(NULL), fIAIndices(new QList<QList<Int_t> >), fIOIndices(new QList<QList<Int_t> >), fOAIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fSelDepProcs(new QList<Bool_t>), fIASProc(new QList<Bool_t>), fOASProc(new QList<Bool_t>), fAPDepends(new QList<QMask>), fAAPDepends(fAPDepends), fObjsPDepends(new QList<QMask>), fAObjsPDepends(fObjsPDepends), fIArrays(new QList<QProcArray*>), fAIArrays(new QList<QProcArray*>), fAAIArrays(fAIArrays), fIObjects(new QList<QProcObj*>), fAIObjects(new QList<QProcObj*>), fAAIObjects(fAIObjects), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>), fBuffers(new QList<void*>), fActiveAIO(NULL), fLastActiveAIO(NULL), fAllIObjects(NULL), fAllOObjects(NULL) {}
    QArrayProcessor(const QArrayProcessor &rhs): QStdProcessor(rhs), fSelProcs(new QList<Bool_t>(*rhs.fSelProcs)), fTrigAEProcs(new QList<Bool_t>(*rhs.fTrigAEProcs)), fNAEProcs(0), fAnalysisDir(rhs.fAnalysisDir), fIANames(new QList<QList<TString> >(*rhs.fIANames)), fAIANames(new QList<QList<TString> >(*rhs.fAIANames)), fOANames(new QList<QList<TString> >(*rhs.fOANames)), fBuNames(new QList<TString>(*rhs.fBuNames)), fActiveAIA(rhs.fActiveAIA?new QMask(*rhs.fActiveAIA):NULL), fLastActiveAIA(rhs.fLastActiveAIA?new QMask(*rhs.fLastActiveAIA):NULL), fIAIndices(new QList<QList<Int_t> >(*rhs.fIAIndices)), fIOIndices(new QList<QList<Int_t> >(*rhs.fIOIndices)), fOAIndices(new QList<QList<Int_t> >(*rhs.fOAIndices)), fOOIndices(new QList<QList<Int_t> >(*rhs.fOOIndices)), fSelDepProcs(new QList<Bool_t>(*rhs.fSelDepProcs)), fIASProc(new QList<Bool_t>(*rhs.fIASProc)), fOASProc(new QList<Bool_t>(*rhs.fOASProc)), fAPDepends(new QList<QMask>(*rhs.fAPDepends)), fAAPDepends(rhs.fAAPDepends==rhs.fAPDepends?fAPDepends:new QList<QMask>(*rhs.fAAPDepends)), fObjsPDepends(new QList<QMask>(*rhs.fObjsPDepends)), fAObjsPDepends(rhs.fAObjsPDepends==rhs.fObjsPDepends?fObjsPDepends:new QList<QMask>(*rhs.fAObjsPDepends)), fIArrays(new QList<QProcArray*>), fAIArrays(new QList<QProcArray*>), fAAIArrays(rhs.fAAIArrays==rhs.fAIArrays?fAIArrays:new QList<QProcArray*>(*rhs.fAAIArrays)), fIObjects(new QList<QProcObj*>(*rhs.fIObjects)), fAIObjects(new QList<QProcObj*>(*rhs.fAIObjects)), fAAIObjects(rhs.fAAIObjects==rhs.fAIObjects?fAIObjects:new QList<QProcObj*>(*rhs.fAAIObjects)), fOArrays(new QList<QProcArray*>), fOObjects(new QList<QProcObj*>(*rhs.fOObjects)), fBuffers(new QList<void*>), fActiveAIO(rhs.fActiveAIO?new QMask(*rhs.fActiveAIO):NULL), fLastActiveAIO(rhs.fLastActiveAIO?new QMask(*rhs.fLastActiveAIO):NULL), fAllIObjects(NULL), fAllOObjects(NULL) {}
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
    void UpdateProcessFlags();

    const QArrayProcessor& operator=(const QArrayProcessor &rhs);

    void PrintAnalysisResults() const;
    void PrintProcesses(const UInt_t &level=0, const Bool_t &printdeps=kTRUE) const;

    void SetAIAActive(const char* name, const char* title = NULL, const Bool_t &active=kTRUE);

    void SetAIOActive(QProcObj *const obj, const Bool_t &active=kTRUE);

    void TerminateProcess();

    void Browse(TBrowser *b);

  protected:
    Int_t AEProcIndexToIndex(const Int_t &index);
    Int_t PSProcIndexToIndex(const Int_t &index);
    void BuildObjLists();
    void ClearObjLists(){if(fAllIObjects) {delete fAllIObjects; fAllIObjects=NULL; delete fAllOObjects; fAllOObjects=NULL;}}
    const QList<QProcObj*>& GetIObjList() const{return *fAllIObjects;}
    const QList<QProcObj*>& GetOObjList() const{return *fAllOObjects;}
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
    QList<QList<TString> >    *fAIANames; //-> Names of absolute input arrays. Order of elements must match fIANames
    QList<QList<TString> >    *fOANames; //-> Names of generated arrays
    QList<TString>            *fBuNames; //-> Names of output buffers
    QMask   		    *fActiveAIA; //! Indicates which absolute input arrays are active within this processor (i.e. not the other processors). NULL if all absolute input objects are active 
    QMask   	        *fLastActiveAIA; //! Indicates which absolute input arrays are active at the moment of the last call to UpdateProcessFlags. NULL if all absolute input objects are active 
    QList<QList<Int_t> >    *fIAIndices; //-> Indices of input arrays that are used as input for each process
    QList<QList<Int_t> >    *fIOIndices; //-> Indices of objects that are used as input for each process
    QList<QList<Int_t> >    *fOAIndices; //-> Indices of arrays that are used as output for each process
    QList<QList<Int_t> >    *fOOIndices; //-> Indices of objects that are used as output for each process
    QList<Bool_t>         *fSelDepProcs; //-> Indicate if a given process depends directly or indirectly on a selector process, or if it is itself a selector process
    QList<Bool_t>             *fIASProc; //-> Direct or indirect dependency of input arrays on a selector process
    QList<Bool_t>             *fOASProc; //-> Direct or indirect dependency of output arrays on a selector process
    QList<QMask>            *fAPDepends; //-> Dependencies of processes on absolute input arrays
    QList<QMask>            *fAAPDepends; //-> Dependencies of processes on active absolute input arrays
    QList<QMask>         *fObjsPDepends; //-> Dependencies of processes on absolute input objects
    QList<QMask>         *fAObjsPDepends; //! Dependencies of processes on active absolute input objects. fAObjsPDepends==fObjsPDepends if all absolute input objects are active
    QList<QProcArray*>        *fIArrays; //! Input arrays
    QList<QProcArray*>        *fAIArrays; //! Absolute input arrays
    QList<QProcArray*>        *fAAIArrays; //! Active absolute input arrays
    QList<QProcObj*>         *fIObjects; //! Input objects 
    QList<QProcObj*>         *fAIObjects; //! Absolute input objects. 
    QList<QProcObj*>         *fAAIObjects; //! Active absolute input objects. fAAIObjects==fAIObjects if all absolute input objects are active 
    QList<QProcArray*>        *fOArrays; //! Output arrays
    QList<QProcObj*>         *fOObjects; //! Output objects 
    QList<void*>              *fBuffers; //! Buffers for output buffers
    QMask   		    *fActiveAIO; //! Indicates which absolute input objects are active within this processor (i.e. not the other processors). NULL if all absolute input objects are active 
    QMask   	        *fLastActiveAIO; //! Indicates which absolute input objects are active at the moment of the last call to UpdateProcessFlags. NULL if all absolute input objects are active 

    QList<QProcObj*>        *fAllIObjects; //! All input objects, including input arrays (temporary)
    QList<QProcObj*>        *fAllOObjects; //! All output objects, including output arrays (temporary)

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
