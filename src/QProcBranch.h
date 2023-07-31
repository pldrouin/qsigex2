// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

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
    QProcBranch(): QProcArray(), TBranch(), fBuffer(NULL), fOwnsBuffer(kFALSE) {}
    QProcBranch(TTree* tree, const char* name, void* address, const char* leaflist, Int_t basketsize = 32000, Int_t compress = -1): QProcArray(), TBranch(tree,name,address,leaflist,basketsize,compress), fBuffer(NULL), fOwnsBuffer(kFALSE){SetBuffer();}
    virtual ~QProcBranch(){ClearBuffer();}
    void ClearBuffer();
    Int_t Fill(){return TBranch::Fill();}
#ifndef __CINT__
    void* const& GetBuffer() const{return fBuffer;}
#endif
    Long64_t GetEntries() const{return TBranch::GetEntries();}
    void LoadEntry(const Long64_t &entry = 0){TBranch::GetEntry(entry);}
    Int_t GetEntry(Long64_t entry = 0, Int_t dummy=0){return TBranch::GetEntry(entry,dummy);}
    void InitProcObj(){ResetArray();}
    void ResetArray(){DeleteBaskets("all");}
    void SetBuffer(void *buffer=NULL);
    void TerminateProcObj(){GetTree()->SetEntries(TBranch::GetEntries());}
    void UnloadArray();

  protected:
    void* fBuffer; //!
    Bool_t fOwnsBuffer; //!

  private:
    QProcBranch(const QProcBranch &): QProcArray(), TBranch(){}
    const QProcBranch& operator=(const QProcBranch&){return *this;}
    ClassDef(QProcBranch,1) //TBranch with QProcObj properties
};

#endif
