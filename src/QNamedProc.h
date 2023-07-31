// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QNAMEDPROC_
#define _QNAMEDPROC_

#include "TROOT.h"
#include "TBrowser.h"
#include "TNamed.h"
#include "TString.h"
#include "QCompProc.h"
//#include "QCINTProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QNamedProc: public TNamed
{
  public:
    QNamedProc(): TNamed(), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>), fProcedure(NULL){}
    QNamedProc(const char *name, const char *title): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",NULL)), fProcedure(NULL){}
    QNamedProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",NULL)), fProcedure(NULL){SetProc(proc,procname);}
//    QNamedProc(const char *name, const char *title, const char *procname): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",procname)) {fProcedure=new QCINTProc(procname);}
//    QNamedProc(const char *name, const char *title, void *proc, const char *procname=NULL): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",procname)) {fProcedure=new QCINTProc(proc);}
    QNamedProc(const QNamedProc &rhs):TNamed(rhs), fIVarsNames(new QList<QNamedVar<TString> >(*rhs.fIVarsNames)), fOVarsNames(new QList<QNamedVar<TString> >(*rhs.fOVarsNames)), fParamsNames(new QList<QNamedVar<TString> >(*rhs.fParamsNames)), fProcName(new QNamedVar<TString>(*rhs.fProcName)), fProcedure(NULL){if(rhs.fProcedure) fProcedure=rhs.fProcedure->Clone();}
    virtual ~QNamedProc(){PRINTF2(this,"\tQNamedProc::~QNamedProc()\n") if(fProcedure) {delete fProcedure; fProcedure=NULL;} delete fIVarsNames; fIVarsNames=NULL; delete fOVarsNames; fOVarsNames=NULL; delete fParamsNames; fParamsNames=NULL; delete fProcName; fProcName=NULL;}

    void AddIVar(const char *name, const char *title=NULL, const Int_t &index=-1, void* const buf=NULL);
    void AddIObj(QProcObj* const obj=NULL, const Int_t &index=-1);
    void AddOVar(const char *name, const char *title=NULL, const Int_t &index=-1, void* const buf=NULL);
    void AddOObj(QProcObj* const obj=NULL, const Int_t &index=-1);
    void AddParam(const char *name, const char* title=NULL, const Int_t &index=-1, Double_t* const buf=NULL);

    void DelIVar(const Int_t &index=-1){fIVarsNames->Del(index); fProcedure->DelIVar(index);}
    void DelIObj(const Int_t &index=-1){fProcedure->DelIObj(index);}
    void DelOVar(const Int_t &index=-1){fOVarsNames->Del(index); fProcedure->DelOVar(index);}
    void DelOObj(const Int_t &index=-1){fProcedure->DelOObj(index);}
    void DelParam(const Int_t &index=-1){fParamsNames->Del(index); fProcedure->DelParam(index);}

    Bool_t Exec() const{return fProcedure->Exec();}

    const char* GetProcName() const{return (TString&)*fProcName;}

    QNamedVar<TString>& GetIVarNameTitle(const Int_t &index) const{return (*fIVarsNames)[index];}
    QNamedVar<TString>& GetOVarNameTitle(const Int_t &index) const{return (*fOVarsNames)[index];}
    QNamedVar<TString>& GetParam(const Int_t &index) const{return (*fParamsNames)[index];}

    const Int_t& GetNIVars() const{return fProcedure->GetNIVars();}
    const Int_t& GetNIObjs() const{return fProcedure->GetNIObjs();}
    const Int_t& GetNOVars() const{return fProcedure->GetNOVars();}
    const Int_t& GetNOObjs() const{return fProcedure->GetNOObjs();}
    const Int_t& GetNParams() const{return fProcedure->GetNParams();}

    const QProcObj* IObj(const Int_t &i) const{return fProcedure->IObj(i);}
    QProcObj* OObj(const Int_t &i) const{return fProcedure->OObj(i);}
    Double_t const& Param(const Int_t &i) const{return fProcedure->Param(i);}

    const QNamedProc& operator=(const QNamedProc& rhs);
    void SetIVarPtr(const Int_t &index, void* const buf, const Int_t &type=-1){fProcedure->SetIVarPtr(index,buf,type);}
    void SetIObjPtr(const Int_t &index, QProcObj* const obj){fProcedure->SetIObjPtr(index,obj);}
    void SetOVarPtr(const Int_t &index, void* const buf, const Int_t &type=-1){fProcedure->SetOVarPtr(index,buf,type);}
    void SetOObjPtr(const Int_t &index, QProcObj* const obj){fProcedure->SetOObjPtr(index,obj);}
    void SetParamPtr(const Int_t &index, Double_t* const buf){fProcedure->SetParamPtr(index,buf);}

    void SetProc(Bool_t (*proc)(QProcArgs&),const char *procname=NULL);
    //void SetProc(const char *procname);
    //void SetProc(void *proc, const char *procname=NULL);

    void TellAddress() const{fProcedure->SetNamedProcAddr(this);}

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

    friend Bool_t operator==(const QNamedProc &lhs, const QNamedProc &rhs);

  protected:
    QList<QNamedVar<TString> > *fIVarsNames; //->
    QList<QNamedVar<TString> > *fOVarsNames; //->
    QList<QNamedVar<TString> > *fParamsNames; //->
    QNamedVar<TString> *fProcName; //->
    QProcedure *fProcedure; //!

  private:

    ClassDef(QNamedProc,1) //QProcedure with named procedure, inputs, outputs and parameters
};

#include "debugger.h"

#endif
