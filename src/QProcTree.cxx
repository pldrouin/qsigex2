#include "QProcTree.h"

ClassImp(QProcTree)

extern TTree *gTree;

TBranch* QProcTree::Branch(const char* name, void* address, const char* leaflist, Int_t bufsize)
{
  gTree = this;
  TBranch* branch = new QProcBranch(this, name, address, leaflist, bufsize);
  if (branch->IsZombie()) {
    delete branch;
    branch = 0;
    return 0;
  }
  fBranches.Add(branch);
  return branch;
}
