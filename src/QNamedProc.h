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
    QNamedProc(): TNamed(), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>), fProcedure(NULL){}
    QNamedProc(const char *name, const char *title): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",NULL)), fProcedure(NULL){}
    QNamedProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",NULL)), fProcedure(NULL){SetProc(proc,procname);}
    QNamedProc(const char *name, const char *title, const char *procname): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",procname)), fProcedure(new QCINTProc(procname)){}
    QNamedProc(const char *name, const char *title, void *proc, const char *procname=NULL): TNamed(name,title), fIVarsNames(new QList<QNamedVar<TString> >), fOVarsNames(new QList<QNamedVar<TString> >), fParamsNames(new QList<QNamedVar<TString> >), fProcName(new QNamedVar<TString>("Procedure",procname)), fProcedure(new QCINTProc(proc)){}
    QNamedProc(const QNamedProc &rhs):TNamed(rhs), fIVarsNames(new QList<QNamedVar<TString> >(*rhs.fIVarsNames)), fOVarsNames(new QList<QNamedVar<TString> >(*rhs.fOVarsNames)), fParamsNames(new QList<QNamedVar<TString> >(*rhs.fParamsNames)), fProcName(new QNamedVar<TString>(*rhs.fProcName)), fProcedure(NULL){if(rhs.fProcedure) fProcedure=rhs.fProcedure->Clone();}
    virtual ~QNamedProc(){PRINTF2(this,"\tQNamedProc::~QNamedProc()\n") if(fProcedure) {delete fProcedure; fProcedure=NULL;} delete fIVarsNames; fIVarsNames=NULL; delete fOVarsNames; fOVarsNames=NULL; delete fParamsNames; fParamsNames=NULL; delete fProcName; fProcName=NULL;}

    void AddIVar(const char *name, const char *title=NULL, Int_t index=-1, Double_t *buf=NULL);
    void AddIObj(QProcObj *obj=NULL, Int_t index=-1);
    void AddOVar(const char *name, const char *title=NULL, Int_t index=-1, Double_t *buf=NULL);
    void AddOObj(QProcObj *obj=NULL, Int_t index=-1);
    void AddParam(const char *name, const char* title=NULL, Int_t index=-1, Double_t *buf=NULL);

    void DelIVar(Int_t index=-1){fIVarsNames->Del(index); fProcedure->DelIVar(index);}
    void DelIObj(Int_t index=-1){fProcedure->DelIObj(index);}
    void DelOVar(Int_t index=-1){fOVarsNames->Del(index); fProcedure->DelOVar(index);}
    void DelOObj(Int_t index=-1){fProcedure->DelOObj(index);}
    void DelParam(Int_t index=-1){fParamsNames->Del(index); fProcedure->DelParam(index);}

    Bool_t Exec() const{return fProcedure->Exec();}

    const char* GetProcName() const{return fProcName->GetValue();}

    QNamedVar<TString>& GetIVarNameTitle(Int_t index) const{return (*fIVarsNames)[index];}
    QNamedVar<TString>& GetOVarNameTitle(Int_t index) const{return (*fOVarsNames)[index];}
    QNamedVar<TString>& GetParam(Int_t index) const{return (*fParamsNames)[index];}

    const Int_t& GetNIVars() const{return fProcedure->GetNIVars();}
    const Int_t& GetNIObjs() const{return fProcedure->GetNIObjs();}
    const Int_t& GetNOVars() const{return fProcedure->GetNOVars();}
    const Int_t& GetNOObjs() const{return fProcedure->GetNOObjs();}
    const Int_t& GetNParams() const{return fProcedure->GetNParams();}

    Double_t& IVar(Int_t i) const{return fProcedure->IVar(i);}
    QProcObj* IObj(Int_t i) const{return fProcedure->IObj(i);}
    Double_t& OVar(Int_t i) const{return fProcedure->OVar(i);}
    QProcObj* OObj(Int_t i) const{return fProcedure->OObj(i);}
    Double_t& Param(Int_t i) const{return fProcedure->Param(i);}

    const QNamedProc& operator=(const QNamedProc& rhs);
    void SetIVarPtr(Int_t index, Double_t *buf){fProcedure->SetIVarPtr(index,buf);}
    void SetIObjPtr(Int_t index, QProcObj *obj){fProcedure->SetIObjPtr(index,obj);}
    void SetOVarPtr(Int_t index, Double_t *buf){fProcedure->SetOVarPtr(index,buf);}
    void SetOObjPtr(Int_t index, QProcObj *obj){fProcedure->SetOObjPtr(index,obj);}
    void SetParamPtr(Int_t index, Double_t *buf){fProcedure->SetParamPtr(index,buf);}

    void SetProc(Bool_t (*proc)(QProcArgs&),const char *procname=NULL);
    void SetProc(const char *procname);
    void SetProc(void *proc, const char *procname=NULL);

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
