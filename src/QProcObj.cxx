// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#include "QProcObj.h"

ClassImp(QProcObj)

Bool_t QProcObj::NewerThan(const TTimeStamp &time) const
{
  if(GetTimeStamp().GetSec() > time.GetSec() || (GetTimeStamp().GetSec() == time.GetSec() && GetTimeStamp().GetNanoSec() > time.GetNanoSec())) return kTRUE;
  return kFALSE;
}
