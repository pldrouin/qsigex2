#ifndef _QTTREEPROCESSOR_
#define _QTTREEPROCESSOR_

#include "QNamedProc.h"
#include "QFileUtils.h"
#include "QMask.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QTTreeProcessor
{
  public:
    QTTreeProcessor(): fProcs(new QList<QNamedProc>), fParams(new QList<Double_t>), fParamsNames(new QList<TString>), fAnalysisDir(new TString), fITNames(new QList<QList<TString> >), fOTNames(new QList<QList<TString> >), fIBNames(new QList<QList<TString> >), fOBNames(new QList<QList<TString> >), fProcsDepends(new QList<QMask>), fOTDepends(new QList<QMask>), fOBDepends(new QList<QList<QMask> >){}
    QTTreeProcessor(const QTTreeProcessor &rhs): fProcs(new QList<QNamedProc>(*rhs.fProcs)), fParams(new QList<Double_t>(*rhs.fParams)), fParamsNames(new QList<TString>(*rhs.fParamsNames)), fAnalysisDir(new TString(*rhs.fAnalysisDir)), fITNames(new QList<QList<TString> >(*rhs.fITNames)), fOTNames(new QList<QList<TString> >(*rhs.fOTNames)), fIBNames(new QList<QList<TString> >(*rhs.fIBNames)), fOBNames(new QList<QList<TString> >(*rhs.fOBNames)), fProcsDepends(new QList<QMask>(*rhs.fProcsDepends)), fOTDepends(new QList<QMask>(*rhs.fOTDepends)), fOBDepends(new QList<QList<QMask> >(*rhs.fOBDepends)){}
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

    Int_t FindParamIndex(const char *paramname) const;
    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNParams(){return fParams->Count();}
    Int_t GetNProcs(){return fProcs->Count();}

    const char* GetParamName(Int_t i) const{return (*fParamsNames)[i];}

    QNamedProc& GetProc(Int_t i) const{return (*fProcs)[i];}
    QNamedProc& GetProc(const char *procname) const;

    const QTTreeProcessor& operator=(const QTTreeProcessor &rhs);

    void PrintAnalysisResults();

    void SetParam(Int_t index=-1, Double_t value=0){fParams[index]=value;}
    void SetParam(const char *paramname, Double_t value=0);
    void SetParams(Double_t *params);

  private:
    QList<QNamedProc> *fProcs; //->
    QList<Double_t> *fParams; //->
    QList<TString> *fParamsNames; //->
    TString *fAnalysisDir;             //-> Directory returned by gDirectory->GetPath() during the last call of Analyze()
    QList<QList<TString> > *fITNames;  //-> Names of existing trees that are used as inputs
    QList<QList<TString> > *fOTNames;  //-> Names of generated trees
    QList<QList<TString> > *fIBNames;  //-> Names of existing branches that are read
    QList<QList<TString> > *fOBNames;  //-> Names of generated branches
    QList<QMask> *fProcsDepends; //-> Dependencies of processes on parameters
    QList<QMask> *fOTDepends; //-> Dependencies of output trees on parameters
    QList<QList<QMask> > *fOBDepends; //-> Dependencies of output branches on parameters
    ClassDef(QTTreeProcessor,1) //The TTree processor class
};

#include "debugger.h"

#endif
