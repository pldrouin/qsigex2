#ifndef _QNAMEDPROC_
#define _QNAMEDPROC_

#include "TROOT.h"
#include "TBrowser.h"
#include "TNamed.h"
#include "TString.h"
#include "QCompProc.h"
#include "QCINTProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QNamedProc: public TNamed
{
  public:
    QNamedProc(): TNamed(), fInputsNames(new QList<QNamedVar<TString> >), fOutputsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>), fProcedure(NULL){}
    QNamedProc(const char *name, const char *title): TNamed(name,title), fInputsNames(new QList<QNamedVar<TString> >), fOutputsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",NULL)), fProcedure(NULL){}
    QNamedProc(const char *name, const char *title, void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname=NULL): TNamed(name,title), fInputsNames(new QList<QNamedVar<TString> >), fOutputsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",NULL)), fProcedure(NULL){SetProc(proc,procname);}
    QNamedProc(const char *name, const char *title, const char *procname): TNamed(name,title), fInputsNames(new QList<QNamedVar<TString> >), fOutputsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",procname)), fProcedure(new QCINTProc(procname)){}
    QNamedProc(const char *name, const char *title, void *proc, const char *procname=NULL): TNamed(name,title), fInputsNames(new QList<QNamedVar<TString> >), fOutputsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",procname)), fProcedure(new QCINTProc(proc)){}
    QNamedProc(const QNamedProc &rhs):TNamed(rhs), fInputsNames(new QList<QNamedVar<TString> >(*(rhs.fInputsNames))), fOutputsNames(new QList<QNamedVar<TString> >(*(rhs.fOutputsNames))), fParamsNames(new QList<QNamedVar<TString> >(*(rhs.fParamsNames))), fProcName(new QNamedVar<TString>(*(rhs.fProcName))), fProcedure(NULL){if(rhs.fProcedure) fProcedure=rhs.fProcedure->Clone();}
    virtual ~QNamedProc(){PRINTF2(this,"\tQNamedProc::~QNamedProc()\n") if(fProcedure) {delete fProcedure; fProcedure=NULL;} delete fInputsNames; fInputsNames=NULL; delete fOutputsNames; fOutputsNames=NULL; delete fParamsNames; fParamsNames=NULL; delete fProcName; fProcName=NULL;}

    void AddInput(const char *name, const char *title=NULL, Int_t index=-1, Double_t *buf=NULL);
    void AddOutput(const char *name, const char *title=NULL, Int_t index=-1, Double_t *buf=NULL);
    void AddParam(const char *name, const char* title=NULL, Int_t index=-1, Double_t *buf=NULL);

    void DelInput(Int_t index=-1){fInputsNames->Del(index); fProcedure->DelInput(index);}
    void DelOutput(Int_t index=-1){fOutputsNames->Del(index); fProcedure->DelOutput(index);}
    void DelParam(Int_t index=-1){fParamsNames->Del(index); fProcedure->DelParam(index);}

    void Exec() const{fProcedure->Exec();}

    const char* GetProcName() const{return fProcName->GetValue();}

    QNamedVar<TString>& GetInput(Int_t index) const{return (*fInputsNames)[index];}
    QNamedVar<TString>& GetOutput(Int_t index) const{return (*fOutputsNames)[index];}
    QNamedVar<TString>& GetParam(Int_t index) const{return (*fParamsNames)[index];}

    Int_t GetNInputs() const{return fInputsNames->Count();}
    Int_t GetNOutputs() const{return fOutputsNames->Count();}
    Int_t GetNParams() const{return fParamsNames->Count();}

    const QNamedProc& operator=(const QNamedProc& rhs);
    void SetInputBuf(Int_t index, Double_t *buf){fProcedure->SetInputBuf(index,buf);}
    void SetOutputBuf(Int_t index, Double_t *buf){fProcedure->SetOutputBuf(index,buf);}
    void SetParamBuf(Int_t index, Double_t *buf){fProcedure->SetParamBuf(index,buf);}

    void SetProc(void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname=NULL);
    void SetProc(const char *procname);
    void SetProc(void *proc, const char *procname=NULL);

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

    friend Bool_t operator==(const QNamedProc &lhs, const QNamedProc &rhs);

  protected:
    QList<QNamedVar<TString> > *fInputsNames; //->
    QList<QNamedVar<TString> > *fOutputsNames; //->
    QList<QNamedVar<TString> > *fParamsNames; //->
    QNamedVar<TString> *fProcName; //->
    QProcedure *fProcedure; //!

  private:

  ClassDef(QNamedProc,1) //Generic named value template class
};

#include "debugger.h"

#endif
