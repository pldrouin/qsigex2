#include "QProcedure.h"

#include "debugger.h"

ClassImp(QProcedure)

Bool_t operator==(const QProcedure &lhs, const QProcedure &rhs)
{
  //Does not compare function pointers
  if(lhs.fInputsBufs!=rhs.fInputsBufs || lhs.fOutputsBufs!=rhs.fOutputsBufs || lhs.fParamsBufs!=rhs.fParamsBufs) return 0;
  return 1;
}

#include "debugger.h"
