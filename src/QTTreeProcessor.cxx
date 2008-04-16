#define DEBUG
#define DEBUG2

#include "QTTreeProcessor.h"

#include "debugger.h"

ClassImp(QTTreeProcessor)

void QTTreeProcessor::AddParam(const char *parname, Int_t index)
{
  fParams.RedimList(fParams.Count()+1,index);
  fParams[index].SetName(parname);
  fParams[index]=0.0;
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
    if(!strcmp(fParams[i].GetName(),paramname)) return i;
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

void QTTreeProcessor::Browse(TBrowser *b)
{
  if(b) {
    b->Add(&fProcs,"Processes");
    b->Add(&fParams,"Input Parameters");
  }
}

#include "debugger.h"
