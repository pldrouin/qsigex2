// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

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
