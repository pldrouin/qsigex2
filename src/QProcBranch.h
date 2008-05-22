#ifndef _QPROCBRANCH_
#define _QPROCBRANCH_

#include "TBranch.h"
#include "QProcObj.h"

class QProcBranch: public TBranch, public QProcObj
{
  public:
    QProcBranch(): TBranch(), QProcObj(){}
    QProcBranch(TTree* tree, const char* name, void* address, const char* leaflist, Int_t basketsize = 32000, Int_t compress = -1);
    virtual ~QProcBranch(){}

  protected:
  private:
    QProcBranch(const QProcBranch &): TBranch(), QProcObj(){}
    ClassDef(QProcBranch,1) //TBranch with QProcObj properties
};

#endif
