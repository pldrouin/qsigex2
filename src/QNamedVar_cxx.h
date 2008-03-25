//#define DEBUG
//#define DEBUG2

#include "debugger.h"

#include "QNamedVar.h"

template <typename U> void QNamedVar<U>::ls(Option_t *) const
{
  TROOT::IndentLevel();
  cout <<"OBJ: " << IsA()->GetName() << "\t" << GetName() << "\t" << GetTitle() << " : "
    << Int_t(TestBit(kCanDelete)) << " at: "<<this<< endl;
}

#include "debugger.h"
