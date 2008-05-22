//#define DEBUG
//#define DEBUG2

#include "QNamedVar.h"

#include "debugger.h"

template <typename U> void QNamedVar<U>::ls(Option_t *) const
{
  TROOT::IndentLevel();
  cout <<"OBJ: " << IsA()->GetName() << "\t" << GetName() << "\t" << GetTitle() << " : "
    << Int_t(TestBit(kCanDelete)) << " at: "<<this<< endl;
}

template<typename V> Bool_t operator==(const QNamedVar<V> &lhs, const QNamedVar<V> &rhs)
{
  if(lhs.fVal != rhs.fVal || lhs.fName != rhs.fName) return kFALSE;
  return kTRUE;
}

#include "debugger.h"
