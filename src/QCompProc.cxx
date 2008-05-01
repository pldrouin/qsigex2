#include "QCompProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QCompProc)

void QCompProc::Exec() const
{
  PRINTF2(this,"\tQCompProc::Exec()\n")
  fProc(fInputsBufs.GetArray(),fOutputsBufs.GetArray(),fParamsBufs.GetArray(),fN);
}

#include "debugger.h"
