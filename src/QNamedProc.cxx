//#define DEBUG
//#define DEBUG2

#include "QNamedProc.h"

#include "debugger.h"

ClassImp(QNamedProc)

void QNamedProc::AddInput(const char *name, const char *title, Int_t index)
{
  QNamedVar<TString> var(name,title);
  fInputsNames.Add(var,index);
  fProcedure->AddInput(index);
}

void QNamedProc::AddOutput(const char *name, const char *title, Int_t index)
{
  QNamedVar<TString> var(name,title);
  fOutputsNames.Add(var,index); 
  fProcedure->AddOutput(index);
}

void QNamedProc::AddParam(const char *name, const char *title, Int_t index)
{
  QNamedVar<TString> var(name,title);
  fParamsNames.Add(var,index);
  fProcedure->AddParam(index);
}

void QNamedProc::Browse(TBrowser *b)
{
  if(b) {
    b->Add(&fInputsNames,"Inputs");
    b->Add(&fOutputsNames,"Outputs");
    b->Add(&fParamsNames,"Parameters");
    b->Add(&fProcName);
  }
}

const QNamedProc& QNamedProc::operator=(const QNamedProc& rhs)
{
  SetNameTitle(rhs.GetName(),rhs.GetTitle());
  fInputsNames=rhs.fInputsNames;
  fOutputsNames=rhs.fOutputsNames;
  fParamsNames=rhs.fParamsNames;
  fProcName=rhs.fProcName;

  if(fProcedure) delete fProcedure;

  if(rhs.fProcedure) fProcedure=rhs.fProcedure->Clone();

  else fProcedure=NULL;
  return *this;
}

void QNamedProc::SetProc(void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname)
{
  fProcName=procname;
  if(fProcedure) delete fProcedure;
  fProcedure=new QCompProc(proc);
}

void QNamedProc::SetProc(const char *procname)
{
  fProcName=procname;
  if(fProcedure) delete fProcedure;
  fProcedure=new QCINTProc(procname);
}

void QNamedProc::SetProc(void *proc, const char *procname)
{
  fProcName=procname;
  if(fProcedure) delete fProcedure;
  fProcedure=new QCINTProc(proc);
}

#include "debugger.h"
