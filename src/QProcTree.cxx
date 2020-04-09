// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QProcTree.h"

ClassImp(QProcTree)

extern TTree *gTree;

TBranch* QProcTree::Branch(const char* name, void* address, const char* leaflist, Int_t bufsize)
{
  TBranch* branch = new QProcBranch(this, name, address, leaflist, bufsize);
  if (branch->IsZombie()) {
    delete branch;
    branch = 0;
    return 0;
  }
  fBranches.Add(branch);
  return branch;
}
