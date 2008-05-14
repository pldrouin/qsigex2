#include "QProcObj.h"

ClassImp(QProcObj)

Bool_t QProcObj::NewerThan(const TTimeStamp &time) const
{
  if(fLastModified.GetSec() > time.GetSec() || (fLastModified.GetSec() == time.GetSec() && fLastModified.GetNanoSec() > time.GetNanoSec())) return kTRUE;
  return kFALSE;
}
