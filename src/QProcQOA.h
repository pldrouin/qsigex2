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
    QProcQOA(): QProcArray(), fArray(NULL), fBuffer(NULL) {}  
    QProcQOA(const char *filename, const char* adesc, QOversizeArray::omode openmode=QOversizeArray::kRead, const UInt_t &qoabuffersize=0, const Int_t &npcbuffers=1, const UInt_t &allocblocksize=0);
    virtual ~QProcQOA(){if(fArray) delete fArray; delete[] fBuffer;}
    Int_t Fill(){fArray->Fill(); return 1;}
    void* GetBuffer() const{return fBuffer;}
    const Int_t& GetBTypeID() const{return fArray->GetObjectTypeID();}
    Long64_t GetEntries() const{return fArray->GetEntries();}
    QOversizeArray* GetQOA(){return fArray;}
    void LoadEntry(const Long64_t &entry = 0){fArray->LoadEntry(entry);}
    const TTimeStamp& GetTimeStamp() const{return fArray->GetTimeStamp();}
    void InitProcObj(){fArray->ResetArray();}
    void ResetArray(){fArray->ResetArray();}
    void SetBuffer(void* buffer){fArray->SetBuffer(buffer);};
    void UnloadArray();
    void UpdateModTime(){fArray->UpdateTimeStamp();}

  protected:
    QOversizeArray *fArray; //!
    Char_t *fBuffer; //!
  private:
    const QProcQOA& operator=(const QProcQOA&){return *this;}
    ClassDef(QProcQOA,1) //QOversizeArray with QProcObj properties
};

#endif
