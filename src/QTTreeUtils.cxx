// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QTTreeUtils.h"

#include "debugger.h"

ClassImp(QTTreeUtils)

void QTTreeUtils::ClearBranchesAddresses(TTree* tree)
{
  //This function the the branch addresses of tree to NULL. It doesn't
  //deallocate memory for the buffer.
  TObjArray* blist=tree->GetListOfBranches();
  for(Int_t j=0;j<blist->GetEntries();j++){
    dynamic_cast<TBranch*>(blist->At(j))->SetAddress(NULL);
  }
}

#include "debugger.h"
