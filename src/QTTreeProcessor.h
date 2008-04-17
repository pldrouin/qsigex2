#ifndef _QTTREEPROCESSOR_
#define _QTTREEPROCESSOR_

#include "QNamedProc.h"
#include "QSigExUtils.h"

#define DEBUG
#define DEBUG2

#include "debugger.h"

class QTTreeProcessor
{
  public:
    QTTreeProcessor(): fProcs(), fParams(), fParamsNames(){}
    QTTreeProcessor(const QTTreeProcessor &rhs): fProcs(rhs.fProcs), fParams(rhs.fParams), fParamsNames(rhs.fParamsNames){}
    virtual ~QTTreeProcessor(){}

    void AddParam(const char *parname, Int_t index=-1);
    void AddProc(const char* name, const char* title=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, const char *procname, Int_t index=-1);
    void AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Int_t index=-1);

    Int_t Analyze();

    void DelParam(Int_t index=-1){fParams.Del(index); fParamsNames.Del(index);}
    void DelParam(const char *paramname);
    void DelProc(Int_t index=-1){fProcs.Del(index);}
    void DelProc(const char *procname);

    Int_t FindParamIndex(const char *paramname) const;
    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNParams(){return fParams.Count();}
    Int_t GetNProcs(){return fProcs.Count();}

    QNamedProc& GetProc(Int_t i) const{return fProcs[i];}
    QNamedProc& GetProc(const char *procname) const;

    const QTTreeProcessor& operator=(const QTTreeProcessor &rhs){return *this;}

    void SetParam(Int_t index=-1, Double_t value=0){fParams[index]=value;}
    void SetParam(const char *paramname, Double_t value=0);
    void SetParams(Double_t *params);

  private:
    QList<QNamedProc> fProcs;
    QList<Double_t> fParams;
    QList<TString> fParamsNames;
    ClassDef(QTTreeProcessor,1)
};

#include "debugger.h"

#endif
