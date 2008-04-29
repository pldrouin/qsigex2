#ifndef _QTTREEPROCESSOR_
#define _QTTREEPROCESSOR_

#include "TFile.h"
#include "TTree.h"
#include "TLeaf.h"
#include "TEventList.h"
#include "QNamedProc.h"
#include "QFileUtils.h"
#include "QMask.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QTTreeProcessor
{
  public:
    QTTreeProcessor(): fProcs(new QList<QNamedProc>), fParams(new QList<Double_t>), fLastParams(new QList<Double_t>), fParamsNames(new QList<TString>), fAnalysisDir(), fITNames(new QList<QList<TString> >), fOTNames(new QList<QList<TString> >), fIBNames(new QList<QList<TString> >), fOBNames(new QList<QList<TString> >), fBuNames(new QList<TString>), fIFiles(new QList<TObject*>), fOFiles(new QList<TObject*>), fIBranches(new QList<QList<TObject*> >), fOBranches(new QList<QList<TObject*> >), fProcsDepends(new QList<QMask>), fIBRequired(new QList<QList<QList<QMask> > >), fIBDepends(new QList<QList<QList<QMask> > >), fOTDepends(new QList<QMask>), fOBDepends(new QList<QList<QMask> >), fIBBuffers(new QList<QList<Double_t> >), fOBBuffers(new QList<QList<Double_t> >), fIBCBuffers(new QList<QList<void*> >), fIBCBTypes(new QList<QList<Char_t> >), fBuffers(new QList<Double_t>) {}
    QTTreeProcessor(const QTTreeProcessor &rhs): fProcs(new QList<QNamedProc>(*rhs.fProcs)), fParams(new QList<Double_t>(*rhs.fParams)), fLastParams(new QList<Double_t>), fParamsNames(new QList<TString>(*rhs.fParamsNames)), fAnalysisDir(rhs.fAnalysisDir), fITNames(new QList<QList<TString> >(*rhs.fITNames)), fOTNames(new QList<QList<TString> >(*rhs.fOTNames)), fIBNames(new QList<QList<TString> >(*rhs.fIBNames)), fOBNames(new QList<QList<TString> >(*rhs.fOBNames)), fBuNames(new QList<TString>(*rhs.fBuNames)), fIFiles(new QList<TObject*>), fOFiles(new QList<TObject*>), fIBranches(new QList<QList<TObject*> >), fOBranches(new QList<QList<TObject*> >), fProcsDepends(new QList<QMask>(*rhs.fProcsDepends)), fIBRequired(new QList<QList<QList<QMask> > >(*rhs.fIBRequired)), fIBDepends(new QList<QList<QList<QMask> > >(*rhs.fIBDepends)), fOTDepends(new QList<QMask>(*rhs.fOTDepends)), fOBDepends(new QList<QList<QMask> >(*rhs.fOBDepends)), fIBBuffers(new QList<QList<Double_t> >), fOBBuffers(new QList<QList<Double_t> >), fIBCBuffers(new QList<QList<void*> >), fIBCBTypes(new QList<QList<Char_t> >), fBuffers(new QList<Double_t>){}
    virtual ~QTTreeProcessor();

    void AddParam(const char *parname, Int_t index=-1);
    void AddProc(const char* name, const char* title=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, const char *procname, Int_t index=-1);
    void AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Int_t index=-1);

    Int_t Analyze();

    void DelParam(Int_t index=-1){fParams->Del(index); fParamsNames->Del(index);}
    void DelParam(const char *paramname);
    void DelProc(Int_t index=-1){fProcs->Del(index); fProcsDepends->Del(index);}
    void DelProc(const char *procname);

    void Exec();

    Int_t FindParamIndex(const char *paramname) const;
    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNParams() const{return fParams->Count();}
    Int_t GetNProcs() const{return fProcs->Count();}

    const char* GetParamName(Int_t i) const{return (*fParamsNames)[i];}

    QNamedProc& GetProc(Int_t i) const{return (*fProcs)[i];}
    QNamedProc& GetProc(const char *procname) const;

    Int_t InitProcess();

    const QTTreeProcessor& operator=(const QTTreeProcessor &rhs);

    void PrintAnalysisResults() const;

    void SetParam(Int_t index=-1, Double_t value=0){fParams[index]=value;}
    void SetParam(const char *paramname, Double_t value=0);
    void SetParams(Double_t *params);

    void TerminateProcess();

  protected:
    void ClearIBCBuffers();

  private:
    QList<QNamedProc> *fProcs;           //-> QNamedProc objects
    QList<Double_t>   *fParams;          //-> Buffers for parameters values
    QList<Double_t>   *fLastParams;      //!  Parameters value from last Exec() call
    QList<TString>    *fParamsNames;     //-> Parameters names
    TString           fAnalysisDir;      //   Directory returned by gDirectory->GetPath() during the last call of Analyze()
    QList<QList<TString> > *fITNames;    //-> Names of trees that are used as inputs
    QList<QList<TString> > *fOTNames;    //-> Names of generated trees
    QList<QList<TString> > *fIBNames;    //-> Names of branches that are read
    QList<QList<TString> > *fOBNames;    //-> Names of generated branches
    QList<TString>         *fBuNames;    //-> Names of output buffers
    QList<TObject*>         *fIFiles;    //! Input files
    QList<TObject*>         *fOFiles;    //! Output files
    QList<QList<TObject*> > *fIBranches; //! Input branches
    QList<QList<TObject*> > *fOBranches; //! Output branches
    QList<QMask>         *fProcsDepends; //-> Dependencies of processes on parameters
    QList<QList<QList<QMask> > > *fIBRequired; //-> Required input branches
    QList<QList<QList<QMask> > > *fIBDepends;  //-> Dependencies of intermediary input branches on parameters
    QList<QMask>         *fOTDepends;    //-> Dependencies of output trees on parameters
    QList<QList<QMask> > *fOBDepends;    //-> Dependencies of output branches on parameters
    QList<QList<Double_t> > *fIBBuffers; //! Buffers for input branches
    QList<QList<Double_t> > *fOBBuffers; //! Buffers for output branches
    QList<QList<void*> >    *fIBCBuffers;//! Buffers for input branches containing a different data type
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

    ClassDef(QTTreeProcessor,1)          //The TTree processor class
};

#include "debugger.h"

#endif
