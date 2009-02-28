#include "QNamedProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QNamedProc)

void QNamedProc::AddIVar(const char *name, const char *title, const Int_t &index, Double_t* const buf)
{
  QNamedVar<TString> var(name,title);
  fIVarsNames->Add(var,index);
  fProcedure->AddIVar(index,buf);
}

void QNamedProc::AddIObj(QProcObj* const obj, const Int_t &index)
{
  fProcedure->AddIObj(index,obj);
}

void QNamedProc::AddOVar(const char *name, const char *title, const Int_t &index, Double_t* const buf)
{
  QNamedVar<TString> var(name,title);
  fOVarsNames->Add(var,index); 
  fProcedure->AddOVar(index,buf);
}

void QNamedProc::AddOObj(QProcObj* const obj, const Int_t &index)
{
  fProcedure->AddOObj(index,obj);
}

void QNamedProc::AddParam(const char *name, const char *title, const Int_t &index, Double_t* const buf)
{
  QNamedVar<TString> var(name,title);
  fParamsNames->Add(var,index);
  fProcedure->AddParam(index,buf);
}

void QNamedProc::Browse(TBrowser *b)
{
  if(b) {
    b->Add(fIVarsNames,"Input Variables");
//    b->Add(const_cast<TObject*>((const TObject*)&(fProcedure->fArgs.GetListOfIObjs())),"Input Objects");
    b->Add(fOVarsNames,"Output Variables");
//    b->Add(const_cast<TObject*>((const TObject*)&(fProcedure->fArgs.GetListOfOObjs())),"Output Objects");
    b->Add(fParamsNames,"Parameters");
    b->Add(fProcName);
  }
}

const QNamedProc& QNamedProc::operator=(const QNamedProc& rhs)
{
  SetNameTitle(rhs.GetName(),rhs.GetTitle());
  *fIVarsNames=*(rhs.fIVarsNames);
  *fOVarsNames=*(rhs.fOVarsNames);
  *fParamsNames=*(rhs.fParamsNames);
  *fProcName=*(rhs.fProcName);

  if(fProcedure) delete fProcedure;

  if(rhs.fProcedure) fProcedure=rhs.fProcedure->Clone();

  else fProcedure=NULL;
  return *this;
}

void QNamedProc::SetProc(Bool_t (*proc)(QProcArgs&),const char *procname)
{
  *fProcName=procname;
  if(fProcedure) delete fProcedure;
  if(G__p2f2funcname((void*)proc)) fProcedure=new QCINTProc((void*)proc);
  else fProcedure=new QCompProc(proc);
}

void QNamedProc::SetProc(const char *procname)
{
  *fProcName=procname;
  if(fProcedure) delete fProcedure;
  fProcedure=new QCINTProc(procname);
}

void QNamedProc::SetProc(void *proc, const char *procname)
{
  *fProcName=procname;
  if(fProcedure) delete fProcedure;
  fProcedure=new QCINTProc(proc);
}

Bool_t operator==(const QNamedProc &lhs, const QNamedProc &rhs)
{
  if((lhs.fProcedure!=NULL) ^ (rhs.fProcedure!=NULL)) return 0;
  if(lhs.fProcedure && rhs.fProcedure && !(*(lhs.fProcedure)==*(rhs.fProcedure))) return 0;
  if(*lhs.fProcName!=*rhs.fProcName || *(lhs.fIVarsNames)!=*(rhs.fIVarsNames) || *(lhs.fOVarsNames)!=*(rhs.fOVarsNames) || *(lhs.fParamsNames)!=*(rhs.fParamsNames)) return 0;
  return 1;
}

#include "debugger.h"
