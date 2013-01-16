// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QProcObj.h"

ClassImp(QProcObj)

Bool_t QProcObj::NewerThan(const TTimeStamp &time) const
{
  if(GetTimeStamp().GetSec() > time.GetSec() || (GetTimeStamp().GetSec() == time.GetSec() && GetTimeStamp().GetNanoSec() > time.GetNanoSec())) return kTRUE;
  return kFALSE;
}
