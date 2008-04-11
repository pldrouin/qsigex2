#include "QCompProc.h"

#include "debugger.h"

ClassImp(QCompProc)

void QCompProc::Exec()
{
  fProc(fInputsBufs.GetArray(),fOutputsBufs.GetArray(),fParamsBufs.GetArray());
}

#include "debugger.h"
