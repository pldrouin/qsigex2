#ifndef _QPROCQOA_
#define _QPROCQOA_

#include "QOversizeArray.h"
#include "QProcArray.h"
#include "QProcBranchHandler.h"
#include "QProcQOAHandler.h"

//The order of base classes is important to ensure that the ROOT dictionary be generated correctly
class QProcQOA: public QProcArray
{
  public:
    QProcQOA(): QProcArray(), fArray(NULL), fBuffer(NULL), fOwnsBuffer(kTRUE) {}  
    QProcQOA(const char *filename, const char* adesc, QOversizeArray::omode openmode=QOversizeArray::kRead, const UInt_t &nobjectsperbuffer=0, const Int_t &npcbuffers=1, const UInt_t &nobjectsallocblock=0);
    virtual ~QProcQOA(){if(fArray) delete fArray; if(fOwnsBuffer) delete[] (Char_t*)fBuffer;}
    Int_t Fill(){fArray->Fill(); return 1;}
    void* const& GetBuffer() const{return fBuffer;}
    Long64_t GetEntries() const{return fArray->GetEntries();}
    QOversizeArray* GetQOA(){return fArray;}
    void LoadEntry(const Long64_t &entry = 0){fArray->LoadEntry(entry);}
    const TTimeStamp& GetTimeStamp() const{return fArray->GetTimeStamp();}
    void InitProcObj(){/*printf("%p: Reseting %p\n",dynamic_cast<QProcObj*>(this),fArray);*/ fArray->ResetArray();}
    void ResetArray(){fArray->ResetArray();}
    void SetBuffer(void* buffer){if(fOwnsBuffer) delete[] (Char_t*)fBuffer; fOwnsBuffer=kFALSE; fBuffer=buffer; fArray->SetBuffer(fBuffer);};
    void UnloadArray();
    void UpdateModTime(){fArray->UpdateTimeStamp();}

  protected:
    QOversizeArray *fArray; //!
    void *fBuffer; //!
    Bool_t fOwnsBuffer; //!
  private:
    const QProcQOA& operator=(const QProcQOA&){return *this;}
    ClassDef(QProcQOA,1) //QOversizeArray with QProcObj properties
};

#endif
