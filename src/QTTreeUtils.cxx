// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

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
