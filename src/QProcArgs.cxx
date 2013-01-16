// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QProcArgs.h"
#include "QNamedProc.h"

ClassImp(QProcArgs)

const Char_t* QProcArgs::GetProcName() const
{
  return fNamedProc->GetName();
}

const QProcArgs& QProcArgs::operator=(const QProcArgs& rhs)
{
  fIBuffers=rhs.fIBuffers;
  fIObjects=rhs.fIObjects;
  fOBuffers=rhs.fOBuffers;
  fOObjects=rhs.fOObjects;
  fPBuffers=rhs.fPBuffers;
  fNamedProc=rhs.fNamedProc;
  return *this;
}

Bool_t operator==(const QProcArgs &lhs, const QProcArgs &rhs)
{
  if(lhs.fIBuffers!=rhs.fIBuffers || lhs.fIObjects!=rhs.fIObjects || lhs.fOBuffers!=rhs.fOBuffers || lhs.fOObjects!=rhs.fOObjects || lhs.fPBuffers!=rhs.fPBuffers || lhs.fNamedProc!=rhs.fNamedProc) return kFALSE;
  return kTRUE;
}
