#ifndef _QNAMEDPROC_
#define _QNAMEDPROC_

#include "TROOT.h"
#include "TBrowser.h"
#include "TNamed.h"
#include "TString.h"
#include "QList.h"
#include "QCompProc.h"
#include "QCINTProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QNamedProc: public TNamed
{
  public:
    QNamedProc(const char *name, const char *title, void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname=NULL): TNamed(name,title), fInputsNames(), fOutputsNames(), fParamsNames(), fProcName("Procedure",procname), fProcedure(new QCompProc(proc)){}
    QNamedProc(const char *name, const char *title, const char *procname): TNamed(name,title), fInputsNames(), fOutputsNames(), fParamsNames(), fProcName("Procedure",procname), fProcedure(new QCINTProc(procname)){}
    QNamedProc(const char *name, const char *title, void *proc, const char *procname=NULL): TNamed(name,title), fInputsNames(), fOutputsNames(), fParamsNames(), fProcName("Procedure",procname), fProcedure(new QCINTProc(proc)){}
    QNamedProc(const QNamedProc &rhs):TNamed(rhs), fInputsNames(rhs.fInputsNames), fOutputsNames(rhs.fOutputsNames), fProcName(rhs.fProcName), fProcedure(NULL){if(rhs.fProcedure) fProcedure=rhs.fProcedure->Clone();}

    void AddInput(const char *name, const char *title=NULL, Int_t index=-1);
    void AddOutput(const char *name, const char *title=NULL, Int_t index=-1);
    void AddParam(const char *name, const char* title=NULL, Int_t index=-1);

    void DelInput(Int_t index=-1){fInputsNames.Del(index); fProcedure->DelInput(index);}
    void DelOutput(Int_t index=-1){fOutputsNames.Del(index); fProcedure->DelOutput(index);}
    void DelParam(Int_t index=-1){fParamsNames.Del(index); fProcedure->DelParam(index);}

    const char* GetProcName() const{return fProcName.GetValue();}

    QNamedVar<TString>& GetInput(Int_t index) const{return fInputsNames[index];}
    QNamedVar<TString>& GetOutput(Int_t index) const{return fOutputsNames[index];}
    QNamedVar<TString>& GetParam(Int_t index) const{return fParamsNames[index];}

    Int_t GetNInputs() const{return fInputsNames.Count();}
    Int_t GetNOutputs() const{return fOutputsNames.Count();}
    Int_t GetNParams() const{return fParamsNames.Count();}

    const QNamedProc& operator=(const QNamedProc& rhs);
    void SetInputBuf(Int_t index, Double_t *buf){fProcedure->SetInputBuf(index,buf);}
    void SetOutputBuf(Int_t index, Double_t *buf){fProcedure->SetOutputBuf(index,buf);}
    void SetParamBuf(Int_t index, Double_t *buf){fProcedure->SetParamBuf(index,buf);}

    void SetProc(void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname=NULL);
    void SetProc(const char *procname);
    void SetProc(void *proc, const char *procname=NULL);

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

    virtual ~QNamedProc(){delete fProcedure;}

  protected:
    QList<QNamedVar<TString> > fInputsNames;
    QList<QNamedVar<TString> > fOutputsNames;
    QList<QNamedVar<TString> > fParamsNames;
    QNamedVar<TString> fProcName;
    QProcedure *fProcedure; //!

  private:
    QNamedProc():TNamed(){}

  ClassDef(QNamedProc,1) //Generic named value template class
};

#include "debugger.h"

#endif
