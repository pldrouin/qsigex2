#define DEBUG
#define DEBUG2

#include "QTTreeProcessor.h"

#include "debugger.h"

ClassImp(QTTreeProcessor)

void QTTreeProcessor::AddParam(const char *parname, Int_t index)
{
  fParamsNames.Add(parname,index);
  fParams.Add(0.0,index);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, Int_t index)
{
  fProcs.RedimList(fProcs.Count()+1,index);
  fProcs[index].SetNameTitle(name,title);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname, Int_t index)
{
  AddProc(name,title,index);
  fProcs[index].SetProc(proc,procname);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, const char *procname, Int_t index)
{
  AddProc(name,title,index);
  fProcs[index].SetProc(procname);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, void *proc, const char *procname, Int_t index)
{
  AddProc(name,title,index);
  fProcs[index].SetProc(proc,procname);
}

Int_t QTTreeProcessor::Analyze()
{
  Int_t i,j;
  QNamedProc *proc;
  QList<TString> etreesfiles;  //Location of trees that already exist
  QList<TString> etreesnames;  //Name of trees that already exist
  TString sbuf;
  TDirectory *curdir=gDirectory;

  for(i=0; i<fProcs.Count(); i++) {
    proc=&(fProcs[i]);

    for(j=0; j<proc->GetNInputs(); j++) {
      sbuf=proc->GetInput(j);

      if(sbuf(0,3) == "[M]") etreesnames=sbuf(4,sbuf.Length());
    }
  }

  return 0;
}

void QTTreeProcessor::DelParam(const char *paramname)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) DelParam(i);
}

void QTTreeProcessor::DelProc(const char *procname)
{
  Int_t i;
  if((i=FindProcIndex(procname))!=-1) fProcs.Del(i);
}

Int_t QTTreeProcessor::FindParamIndex(const char *paramname) const
{
  for(Int_t i=0; i<fParams.Count(); i++){
    if(!strcmp(fParamsNames[i],paramname)) return i;
  }
  return -1;
}

Int_t QTTreeProcessor::FindProcIndex(const char *procname) const
{
  for(Int_t i=0; i<fProcs.Count(); i++){
    if(!strcmp(fProcs[i].GetName(),procname)) return i;
  }
  return -1;
}

QNamedProc& QTTreeProcessor::GetProc(const char *procname) const
{
  Int_t i;
  if((i=FindProcIndex(procname))!=-1){
    return fProcs[i];
  }
  fprintf(stderr,"QTTreeProcessor::GetProc: Procedure '%s' does not exist\n",procname);
  throw 1;
  return fProcs[0];
}

void QTTreeProcessor::SetParam(const char *paramname, Double_t value)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) SetParam(i,value);
}

void QTTreeProcessor::SetParams(Double_t *params)
{
  memcpy(fParams.GetArray(),params,fParams.Count()*sizeof(Double_t));
}

#include "debugger.h"
