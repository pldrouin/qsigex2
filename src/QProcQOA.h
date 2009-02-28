#ifndef _QPROCQOA_
#define _QPROCQOA_

#include "QOversizeArray.h"
#include "QProcArray.h"
#include "QProcBranchHandler.h"
#include "QProcQOAHandler.h"

//The order of base classes is important to ensure that the ROOT dictionary be generated correctly
class QProcQOA: public QProcArray, public QOversizeArray
{
  public:
    QProcQOA(): QProcArray(), QOversizeArray(), fBuffer(NULL) {}  
    QProcQOA(const char *filename, const char* name, QOversizeArray::omode openmode=QOversizeArray::kRead, const UInt_t &objectsize=0, const UInt_t &nobjectsperbuffer=0, const Int_t &npcbuffers=1, const UInt_t &nobjectsallocblock=0);
    virtual ~QProcQOA(){delete[] fBuffer;}
    Int_t Fill(){QOversizeArray::Fill(); return sizeof(Double_t);}
    void* GetBuffer() const{return fBuffer;}
    Long64_t GetEntries() const{return QOversizeArray::GetEntries();}
    void LoadEntry(const Long64_t &entry = 0){QOversizeArray::LoadEntry(entry);}
    const TTimeStamp& GetTimeStamp() const{return QOversizeArray::GetTimeStamp();}
    void InitProcObj(){QOversizeArray::ResetArray();}
    void ResetArray(){QOversizeArray::ResetArray();}
    void SetBuffer(void* buffer){QOversizeArray::SetBuffer(buffer);};
    void UnloadArray();
    void UpdateModTime(){QOversizeArray::UpdateTimeStamp();}

  protected:
    Char_t *fBuffer; //!
  private:
    const QProcQOA& operator=(const QProcQOA&){return *this;}
    ClassDef(QProcQOA,1) //QOversizeArray with QProcObj properties
};

#endif
