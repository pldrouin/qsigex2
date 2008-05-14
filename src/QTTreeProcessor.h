#ifndef _QTTREEPROCESSOR_
#define _QTTREEPROCESSOR_

#include "TNamed.h"
#include "TFile.h"
#include "TLeaf.h"
#include "TEntryList.h"
#include "QProcTree.h"
#include "QNamedProc.h"
#include "QFileUtils.h"
#include "QMask.h"
#include "QDisTH.h"
#include "QProgress.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QTTreeProcessor: public TNamed
{
  public:
    QTTreeProcessor(): TNamed(), fProcs(new QList<QNamedProc>), fSelProcs(new QList<Bool_t>), fNAEProcs(0), fParams(new QList<Double_t>), fLastParams(new QList<Double_t>), fParamsNames(new QList<TString>), fAnalysisDir(), fITNames(new QList<QList<TString> >), fOTNames(new QList<QList<TString> >), fIBNames(new QList<QList<TString> >), fOBNames(new QList<QList<TString> >), fBuNames(new QList<TString>), fITIndices(new QList<QList<Int_t> >), fIBIndices(new QList<QList<Int_t> >), fOTIndices(new QList<QList<Int_t> >), fOBIndices(new QList<QList<Int_t> >), fAITrees(new QList<Bool_t>), fSelDepProcs(new QList<Bool_t>), fITSProc(new QList<Bool_t>), fOTSProc(new QList<Bool_t>), fProcsParDepends(new QList<QMask>), fBPDepends(new QList<QList<QMask> >), fObjsPDepends(new QList<QList<QMask> >), fIFiles(new QList<TObject*>), fOFiles(new QList<TObject*>), fIBranches(new QList<QList<TObject*> >), fIObjects(new QList<QList<QProcObj*> >), fOBranches(new QList<QList<TObject*> >), fOObjects(new QList<QList<QProcObj*> >), fIBBuffers(new QList<QList<Double_t*> >), fOwnsIBBuffers(new QList<QList<Bool_t> >), fOBBuffers(new QList<QList<Double_t> >), fIBCBuffers(new QList<QList<void*> >), fOwnsIBCBuffers(new QList<QList<Bool_t> >), fIBCBTypes(new QList<QList<Char_t> >), fBuffers(new QList<Double_t>) {}
    QTTreeProcessor(const QTTreeProcessor &rhs): TNamed(rhs), fProcs(new QList<QNamedProc>(*rhs.fProcs)), fSelProcs(new QList<Bool_t>(*rhs.fSelProcs)), fNAEProcs(0), fParams(new QList<Double_t>(*rhs.fParams)), fLastParams(new QList<Double_t>), fParamsNames(new QList<TString>(*rhs.fParamsNames)), fAnalysisDir(rhs.fAnalysisDir), fITNames(new QList<QList<TString> >(*rhs.fITNames)), fOTNames(new QList<QList<TString> >(*rhs.fOTNames)), fIBNames(new QList<QList<TString> >(*rhs.fIBNames)), fOBNames(new QList<QList<TString> >(*rhs.fOBNames)), fBuNames(new QList<TString>(*rhs.fBuNames)), fITIndices(new QList<QList<Int_t> >(*rhs.fITIndices)), fIBIndices(new QList<QList<Int_t> >(*rhs.fIBIndices)), fOTIndices(new QList<QList<Int_t> >(*rhs.fOTIndices)), fOBIndices(new QList<QList<Int_t> >(*rhs.fOBIndices)), fAITrees(new QList<Bool_t>(*rhs.fAITrees)), fSelDepProcs(new QList<Bool_t>(*rhs.fSelDepProcs)), fITSProc(new QList<Bool_t>(*rhs.fITSProc)), fOTSProc(new QList<Bool_t>(*rhs.fOTSProc)), fProcsParDepends(new QList<QMask>(*rhs.fProcsParDepends)), fBPDepends(new QList<QList<QMask> >(*rhs.fBPDepends)), fObjsPDepends(new QList<QList<QMask> >(*rhs.fObjsPDepends)), fIFiles(new QList<TObject*>), fOFiles(new QList<TObject*>), fIBranches(new QList<QList<TObject*> >), fIObjects(new QList<QList<QProcObj*> >(*rhs.fIObjects)), fOBranches(new QList<QList<TObject*> >), fOObjects(new QList<QList<QProcObj*> >(*rhs.fOObjects)), fIBBuffers(new QList<QList<Double_t*> >), fOwnsIBBuffers(new QList<QList<Bool_t> >), fOBBuffers(new QList<QList<Double_t> >), fIBCBuffers(new QList<QList<void*> >), fOwnsIBCBuffers(new QList<QList<Bool_t> >), fIBCBTypes(new QList<QList<Char_t> >), fBuffers(new QList<Double_t>){}
    virtual ~QTTreeProcessor();

    void AddParam(const char *parname, Double_t value=0, Int_t index=-1);
    void AddProc(const char* name, const char* title=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    void AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    void AddProc(const char *name, const char *title, const char *procname, Bool_t selector=kFALSE, Int_t index=-1);
    void AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Bool_t selector=kFALSE, Int_t index=-1);
    void AddPSProc(const char* name, const char* title=NULL, Int_t index=-1);
    void AddPSProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Int_t index=-1);
    void AddPSProc(const char *name, const char *title, const char *procname, Int_t index=-1);
    void AddPSProc(const char *name, const char *title, void *proc, const char *procname=NULL, Int_t index=-1);

    void Analyze();

    void DelParam(Int_t index=-1){fParams->Del(index); fParamsNames->Del(index);}
    void DelParam(const char *paramname);
    void DelProc(Int_t index=-1){if(index==-1) index=fProcs->Count()-1; fProcs->Del(index); fSelProcs->Del(index); if(index < fNAEProcs) fNAEProcs--;}
    void DelProc(const char *procname);

    void Exec();

    Int_t FindParamIndex(const char *paramname) const;
    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNParams() const{return fParams->Count();}
    Int_t GetNProcs() const{return fProcs->Count();}
    Int_t GetNPSProcs() const{return fProcs->Count()-fNAEProcs;} //Number of post selection processes

    const char* GetParamName(Int_t i) const{return (*fParamsNames)[i];}

    QNamedProc& GetProc(Int_t index) const{return (*fProcs)[index];}
    QNamedProc& GetProc(const char *procname) const;

    Int_t InitProcess();

    const QTTreeProcessor& operator=(const QTTreeProcessor &rhs);

    void PrintAnalysisResults() const;

    void SetParam(Int_t index=-1, Double_t value=0){(*fParams)[index]=value;}
    void SetParam(const char *paramname, Double_t value=0);
    void SetParams(Double_t *params);

    void TerminateProcess();

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

  protected:
    void ClearIBBuffers();
    void ClearIBCBuffers();
    Int_t AEProcIndexToIndex(Int_t index);
    Int_t PSProcIndexToIndex(Int_t index);

  private:
    QList<QNamedProc> *fProcs;           //-> QNamedProc objects
    QList<Bool_t>     *fSelProcs;        //-> List of selector processes
    Int_t             fNAEProcs;         //   Number of processes that process all entries in the trees
    QList<Double_t>   *fParams;          //-> Buffers for parameters values
    QList<Double_t>   *fLastParams;      //!  Parameters value from last Exec() call
    QList<TString>    *fParamsNames;     //-> Parameters names
    TString           fAnalysisDir;      //   Directory returned by gDirectory->GetPath() during the last call of Analyze()
    QList<QList<TString> > *fITNames;    //-> Names of trees that are used as inputs
    QList<QList<TString> > *fOTNames;    //-> Names of generated trees
    QList<QList<TString> > *fIBNames;    //-> Names of branches that are read
    QList<QList<TString> > *fOBNames;    //-> Names of generated branches
    QList<TString>         *fBuNames;    //-> Names of output buffers
    QList<QList<Int_t> >   *fITIndices;  //-> Indices of trees that are used as input for each process
    QList<QList<Int_t> > *fIBIndices;    //-> Indices of branches that are used as input for each process
    QList<QList<Int_t> >   *fOTIndices;  //-> Indices of trees that are used as output for each process
    QList<QList<Int_t> > *fOBIndices;    //-> Indices of branches that are used as output for each process
    QList<Bool_t>        *fAITrees;       //-> Indicate which trees are absolute inputs to the current instance
    QList<Bool_t>        *fSelDepProcs;  //-> Indicate if a given process depends directly or indirectly on a selector process, or if it is itself a selector process
    QList<Bool_t> *fITSProc;     //-> Dependency of input trees on a selector process
    QList<Bool_t> *fOTSProc;     //-> Dependency of output trees on a selector process
    QList<QMask>           *fProcsParDepends; //-> Dependencies of processes on parameters
    QList<QList<QMask > >   *fBPDepends;    //-> Dependencies of processes on input branches
    QList<QList<QMask > >   *fObjsPDepends; //-> Dependencies of processes on input objects
    QList<TObject*>         *fIFiles;    //! Input files
    QList<TObject*>         *fOFiles;    //! Output files
    QList<QList<TObject*> > *fIBranches; //! Input branches
    QList<QList<QProcObj*> > *fIObjects; //! Input objects 
    QList<QList<TObject*> > *fOBranches; //! Output branches
    QList<QList<QProcObj*> > *fOObjects; //! Output objects 
    QList<QList<Double_t*> > *fIBBuffers; //! Buffers for input branches
    QList<QList<Bool_t> >    *fOwnsIBBuffers; //! Identifies input branch buffers owned by the class
    QList<QList<Double_t> > *fOBBuffers; //! Buffers for output branches
    QList<QList<void*> >    *fIBCBuffers;//! Buffers for input branches containing a different data type
    QList<QList<Bool_t> >    *fOwnsIBCBuffers; //! Identifies input branch buffers containing a different data type owned by the class
    QList<QList<Char_t> >    *fIBCBTypes; //! Data type id of input branches containing a different data type
    QList<Double_t>         *fBuffers;   //! Buffers for output buffers
    enum {
      kDouble_t,
      kFloat_t,
      kUInt_t,
      kInt_t,
      kUShort_t,
      kShort_t,
      kUChar_t,
      kChar_t,
      kBool_t
    };

    class QDependentProcs
    {
      public:
	QDependentProcs():fIdx(fQPDObjs.Count()),fDepends(){fQPDObjs.Add(this);}
	virtual ~QDependentProcs(){fQPDObjs.Del(fIdx);}
	QList<Int_t> GetAllDepends() const;
	void AddDepend(Int_t pidx){if(fDepends.FindFirst(pidx) == -1 && pidx != fIdx) fDepends.Add(pidx);}
	void DelDepend(Int_t pidx){Int_t idx=fDepends.FindFirst(pidx); if(idx!=-1) fDepends.Del(idx);}
	Int_t GetNDepends(){return fDepends.Count();}
      private:
	QDependentProcs(const QDependentProcs&){};
	const QDependentProcs& operator=(const QDependentProcs&){return *this;}

	Int_t fIdx;
	QList<Int_t> fDepends;
	static Int_t fInitIdx;
	static QList<void*> fQPDObjs;
	static QList<Int_t> fPCalled;
    };

    ClassDef(QTTreeProcessor,1)          //The TTree processor class
};

#include "debugger.h"

#endif
