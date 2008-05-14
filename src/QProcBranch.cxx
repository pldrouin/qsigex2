#include "QProcBranch.h"

ClassImp(QProcBranch)

QProcBranch::QProcBranch(TTree* tree, const char* name, void* address, const char* leaflist, Int_t basketsize, Int_t compress): TBranch(tree,name,address,leaflist,basketsize,compress), QProcObj()
{
}
