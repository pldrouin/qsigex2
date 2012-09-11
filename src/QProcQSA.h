#ifndef _QPROCQSA_
#define _QPROCQSA_

#include "QSharedArray.h"
#include "QProcArray.h"
#include "QProcBranchHandler.h"
#include "QProcQOAHandler.h"

//The order of base classes is important to ensure that the ROOT dictionary be generated correctly
class QProcQSA: public QProcArray
{
  public:
    QProcQSA(): QProcArray(), fArray(NULL), fBuffer(NULL), fOwnsBuffer(kTRUE) {}  
    QProcQSA(const char *filename, const char* adesc, const UInt_t &nobjectsperbuffer=0);
    virtual ~QProcQSA(){if(fArray) delete fArray; if(fOwnsBuffer) delete[] (Char_t*)fBuffer;}
    Int_t Fill(){fArray->Fill(); return 1;}
#ifndef __CINT__
    void* const& GetBuffer() const{return fBuffer;}
#endif
    Long64_t GetEntries() const{return fArray->GetEntries();}
    QSharedArray* GetQOA(){return fArray;}
    void LoadEntry(const Long64_t &entry = 0){fArray->LoadEntry(entry);}
    const TTimeStamp& GetTimeStamp() const{return fArray->GetTimeStamp();}
    void InitProcObj(){fprintf(stderr,"Error: QProcQOA::InitProObj(): Illegal operation on a read-only object\n"); throw 1;}
    void ResetArray(){fprintf(stderr,"Error: QProcQOA::InitProObj(): Illegal operation on a read-only object\n"); throw 1;}
    void SetBuffer(void* buffer){if(fOwnsBuffer) delete[] (Char_t*)fBuffer; fOwnsBuffer=kFALSE; fBuffer=buffer; fArray->SetBuffer(fBuffer);};
    void UnloadArray();
    void UpdateModTime(){fArray->UpdateTimeStamp();}

  protected:
    QSharedArray *fArray; //!
    void *fBuffer; //!
    Bool_t fOwnsBuffer; //!
  private:
    const QProcQSA& operator=(const QProcQSA&){return *this;}
    ClassDef(QProcQSA,1) //QSharedArray with QProcObj properties
};

#endif
