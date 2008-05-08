#include "QCompProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QCompProc)

Bool_t QCompProc::Exec()
{
  PRINTF2(this,"\tQCompProc::Exec()\n")
  return fProc(fArgs);
}

#include "debugger.h"
