#include "QProcArgs.h"

ClassImp(QProcArgs)

const QProcArgs& QProcArgs::operator=(const QProcArgs& rhs)
{
  fIBuffers=rhs.fIBuffers;
  fIObjects=rhs.fIObjects;
  fOBuffers=rhs.fOBuffers;
  fOObjects=rhs.fOObjects;
  fPBuffers=rhs.fPBuffers;
  return *this;
}

Bool_t operator==(const QProcArgs &lhs, const QProcArgs &rhs)
{
  if(lhs.fIBuffers!=rhs.fIBuffers || lhs.fIObjects!=rhs.fIObjects || lhs.fOBuffers!=rhs.fOBuffers || lhs.fOObjects!=rhs.fOObjects || lhs.fPBuffers!=rhs.fPBuffers) return kFALSE;
  return kTRUE;
}
