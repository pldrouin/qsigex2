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
    QProcBranch(): QProcArray(), TBranch(), fBuffer(NULL), fOwnsBuffer(kFALSE), fOwnsCBuffer(kFALSE), fCBType(0), fCBuffer(NULL) {}
    QProcBranch(TTree* tree, const char* name, void* address, const char* leaflist, Int_t basketsize = 32000, Int_t compress = -1);
    virtual ~QProcBranch();
    void ClearBuffer();
    Int_t Fill();
    void* GetBuffer() const{return fBuffer;}
    const Int_t& GetBTypeID() const{return cBType;}
    Long64_t GetEntries() const{return TBranch::GetEntries();}
    void LoadEntry(const Long64_t &entry = 0);
    Int_t GetEntry(Long64_t entry = 0, Int_t dummy=0){LoadEntry(entry); return 0;}
    void InitProcObj(){ResetArray();}
    void ResetArray(){DeleteBaskets("all");}
    void SetBuffer(void *buffer=NULL);
    void TerminateProcObj(){GetTree()->SetEntries(TBranch::GetEntries());}
    void UnloadArray();

  protected:
    Double_t *fBuffer; //!
    Bool_t fOwnsBuffer; //!
    Bool_t fOwnsCBuffer; //!
    Char_t fCBType; //!
    void *fCBuffer; //!
    static const Int_t cBType=8; //!
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
