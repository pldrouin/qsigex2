#ifndef _QPROCTBRANCHWRAPPER_
#define _QPROCTBRANCHWRAPPER_

#include "TDirectory.h"
#include "TTree.h"
#include "TBranch.h"
#include "TLeaf.h"
#include "QProcArray.h"
#include "QProcBranchHandler.h"

class QProcTBranchWrapper: public QProcArray
{
  public:
    QProcTBranchWrapper(TBranch *branch): QProcArray(), fBranch(branch), fBuffer(NULL), fOwnsBuffer(kFALSE), fOwnsCBuffer(kFALSE), fCBType(0), fCBuffer(NULL) {SetBuffer();}
    virtual ~QProcTBranchWrapper();
    void ClearBuffer();
    Int_t Fill();
    TBranch* GetBranch(){return fBranch;}
    void* GetBuffer() const{return fBuffer;}
    Long64_t GetEntries() const{return fBranch->GetEntries();}
    Int_t GetEntry(Long64_t entry = 0, Int_t dummy=0);
    void InitProcObj(){ResetArray();}
    Bool_t NewerThan(const TTimeStamp &) const{return kFALSE;}
    void UpdateModTime(){}
    void ResetArray(){fBranch->DeleteBaskets("all");}
    void SetBuffer(void *buffer=NULL);
    void TerminateProcObj(){fBranch->GetTree()->SetEntries(fBranch->GetEntries());}
    void UnloadArray();

  protected:
    TBranch *fBranch; //!
    Double_t *fBuffer; //!
    Bool_t fOwnsBuffer; //!
    Bool_t fOwnsCBuffer; //!
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
    QProcTBranchWrapper() {}
    QProcTBranchWrapper(const QProcTBranchWrapper &): QProcArray(){}
    const QProcTBranchWrapper& operator=(const QProcTBranchWrapper&){return *this;}
    ClassDef(QProcTBranchWrapper,1) //Wrapper around TBranch class to give it QProcArray properties
};

#endif
