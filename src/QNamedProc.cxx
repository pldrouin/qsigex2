#include "QNamedProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QNamedProc)

void QNamedProc::AddInput(const char *name, const char *title, Int_t index, Double_t *buf)
{
  QNamedVar<TString> var(name,title);
  fInputsNames->Add(var,index);
  fProcedure->AddInput(index,buf);
}

void QNamedProc::AddOutput(const char *name, const char *title, Int_t index, Double_t *buf)
{
  QNamedVar<TString> var(name,title);
  fOutputsNames->Add(var,index); 
  fProcedure->AddOutput(index,buf);
}

void QNamedProc::AddParam(const char *name, const char *title, Int_t index, Double_t *buf)
{
  QNamedVar<TString> var(name,title);
  fParamsNames->Add(var,index);
  fProcedure->AddParam(index,buf);
}

void QNamedProc::Browse(TBrowser *b)
{
  if(b) {
    b->Add(fInputsNames,"Inputs");
    b->Add(fOutputsNames,"Outputs");
    b->Add(fParamsNames,"Parameters");
    b->Add(fProcName);
  }
}

const QNamedProc& QNamedProc::operator=(const QNamedProc& rhs)
{
  SetNameTitle(rhs.GetName(),rhs.GetTitle());
  *fInputsNames=*(rhs.fInputsNames);
  *fOutputsNames=*(rhs.fOutputsNames);
  *fParamsNames=*(rhs.fParamsNames);
  *fProcName=*(rhs.fProcName);

  if(fProcedure) delete fProcedure;

  if(rhs.fProcedure) fProcedure=rhs.fProcedure->Clone();

  else fProcedure=NULL;
  return *this;
}

void QNamedProc::SetProc(Bool_t (*proc)(Double_t**, Double_t**, Double_t**, const Int_t*),const char *procname)
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
  if(lhs.fProcedure!=NULL ^ rhs.fProcedure!=NULL) return 0;
  if(lhs.fProcedure && rhs.fProcedure && !(*(lhs.fProcedure)==*(rhs.fProcedure))) return 0;
  if(*(lhs.fProcName)!=*(rhs.fProcName) || *(lhs.fInputsNames)!=*(rhs.fInputsNames) || *(lhs.fOutputsNames)!=*(rhs.fOutputsNames) || *(lhs.fParamsNames)!=*(rhs.fParamsNames)) return 0;
  return 1;
}

#include "debugger.h"
