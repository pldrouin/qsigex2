// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

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
