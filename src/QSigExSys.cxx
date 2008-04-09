//#define DEBUG
//#define DEBUG2

#include "QSigExSys.h"

//#include "debugger.h"

ClassImp(QSigExSys)

void QSigExSys::AddInput(const char *tree, const char *branch, Int_t index)
{
  QNamedVar<TString> var(branch,tree);
  fInputsNames.Add(var,index);
}

void QSigExSys::AddOutput(const char *tree, const char *branch, Int_t index)
{
  QNamedVar<TString> var(branch,tree);
  fOutputsNames.Add(var,index); 
}

void QSigExSys::AddArg(const char *name, Int_t index)
{
  QNamedVar<TString> var(name,"");
  fArgsNames.Add(var,index);
}

void QSigExSys::Browse(TBrowser *b)
{
  if(b) {
    b->Add(&fInputsNames,"Inputs");
    b->Add(&fOutputsNames,"Outputs");
    b->Add(&fArgsNames,"Arguments");
  }
}

//#include "debugger.h"
