#include "QCINTProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QCINTProc)

Bool_t QCINTProc::Exec() const
{
  Long_t ret;
  PRINTF2(this,"\tQCINTProc::Exec()\n")
  fMethodCall->Execute(&ret);
  return (Bool_t)ret;
}

void QCINTProc::InitArgs()
{
  if(fMethodCall) {
    Long_t args[4];
    args[0]=(Long_t)fInputsBufs.GetArray();
    args[1]=(Long_t)fOutputsBufs.GetArray();
    args[2]=(Long_t)fParamsBufs.GetArray();
    args[3]=(Long_t)fN;
    fMethodCall->SetParamPtrs(args);
  }
}

void QCINTProc::SetProc(void *proc)
{
  const char *procname = G__p2f2funcname(proc);

  if(procname) SetProc(procname);

  else {
    fprintf(stderr,"QCINTProc::SetProc(void *proc<%p>): Cannot find any function at this address\n",proc);
  }
}

void QCINTProc::SetProc(const char *procname)
{
  if(fMethodCall) delete fMethodCall;
  fMethodCall=new TMethodCall;
  fMethodCall->InitWithPrototype(procname,"Double_t**,Double_t**,Double_t**,const Int_t*");
  InitArgs();
  if(!fMethodCall->IsValid()) {
    fprintf(stderr,"QCINTProc::SetProc(const char *procname<'%s'>): No function found with the signature %s(const Double_t**, const Double_t**, Double_t**, const Int_t*)\n",procname,procname);
  }
}

#include "debugger.h"
