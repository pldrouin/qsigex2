#ifndef _QPROCBRANCH_
#define _QPROCBRANCH_

#include "TDirectory.h"
#include "TTree.h"
#include "TBranch.h"
#include "TLeaf.h"
#include "QProcArray.h"
#include "QProcBranchHandler.h"

//The order of base classes is important to ensure that the ROOT dictionary be generated correctly
class QProcBranch: public QProcArray, public TBranch
{
  public:
    QProcBranch(): QProcArray(), TBranch(), fBuffer(NULL), fOwnsBuffer(kFALSE), fCBType(0), fCBuffer(NULL) {}
    QProcBranch(TTree* tree, const char* name, void* address, const char* leaflist, Int_t basketsize = 32000, Int_t compress = -1);
    virtual ~QProcBranch();
    Int_t Fill();
    Double_t* GetBuffer() const{return fBuffer;}
    Int_t GetEntry(Long64_t entry = 0, Int_t dummy=0);
    void InitProcObj(){ResetArray();}
    void ResetArray(){DeleteBaskets("all");}
    void TerminateProcObj(){UpdateModTime();}
    void UnloadArray();

  protected:
    void SetBuffer();
    Double_t *fBuffer; //!
    Bool_t fOwnsBuffer; //!
    Char_t fCBType; //!
    void *fCBuffer; //!
    enum {
      kDouble_t,
      kFloat_t,
      kUInt_t,
      kInt_t,
      kUShort_t,
      kShort_t,
      kUChar_t,
      kChar_t,
      kBool_t
    };
  private:
    QProcBranch(const QProcBranch &): QProcArray(), TBranch(){}
    const QProcBranch& operator=(const QProcBranch&){return *this;}
    ClassDef(QProcBranch,1) //TBranch with QProcObj properties
};

#endif
