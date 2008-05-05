#include "QCompProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QCompProc)

Bool_t QCompProc::Exec() const
{
  PRINTF2(this,"\tQCompProc::Exec()\n")
  return fProc(fInputsBufs.GetArray(),fOutputsBufs.GetArray(),fParamsBufs.GetArray(),fN);
}

#include "debugger.h"
