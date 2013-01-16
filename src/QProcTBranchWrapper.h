// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

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
    QProcTBranchWrapper(TBranch *branch): QProcArray(), fBranch(branch), fBuffer(NULL), fOwnsBuffer(kFALSE) {SetBuffer(); UpdateModTime();}
    virtual ~QProcTBranchWrapper(){ClearBuffer();}
    void ClearBuffer();
    Int_t Fill(){return fBranch->Fill();}
    TBranch* GetBranch(){return fBranch;}
#ifndef __CINT__
    void* const& GetBuffer() const{return fBuffer;}
#endif
    Long64_t GetEntries() const{return fBranch->GetEntries();}
    void LoadEntry(const Long64_t &entry = 0){fBranch->GetEntry(entry);}
    void InitProcObj(){ResetArray();}
    void ResetArray(){fBranch->DeleteBaskets("all");}
    void SetBuffer(void *buffer=NULL);
    void TerminateProcObj(){fBranch->GetTree()->SetEntries(fBranch->GetEntries());}
    void UnloadArray();

  protected:
    TBranch *fBranch; //!
    void *fBuffer; //!
    Bool_t fOwnsBuffer; //!

  private:
    QProcTBranchWrapper() {}
    QProcTBranchWrapper(const QProcTBranchWrapper &): QProcArray(){}
    const QProcTBranchWrapper& operator=(const QProcTBranchWrapper&){return *this;}
    ClassDef(QProcTBranchWrapper,1) //Wrapper around TBranch class to give it QProcArray properties
};

#endif
