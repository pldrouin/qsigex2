#ifndef _OVERSIZEARRAY_
#define _OVERSIZEARRAY_

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include "Rtypes.h"
#include "TString.h"
#include "QOABuffer.h"

class QOversizeArray
{
  public:
    enum omode{kRead, kRW, kRecreate};
    QOversizeArray(const char *filename, const char *arrayname, omode openmode=kRead, const UInt_t &objectsize=0, const UInt_t &nobjectsperbuffer=1024, const Long64_t &arraymaxmemsize=-1, const Long64_t &allarraysmemmaxsize=-2);
    virtual ~QOversizeArray();

    void CloseFile();

    void Fill();

    void* GetBuffer() const{return fBuffer;}
    Long64_t GetNObjects() const{return fNObjects;}

    void OpenFile();

    void Save();

    void SetBuffer(void *buffer){fBuffer=buffer;}

  protected:
    void CheckMemory(Bool_t removefromleft=kTRUE);
    void Init();
    void Read(void *buf, const size_t &size, const size_t &num, const Long64_t &pos=-1) const;
    void ReadHeader();
    void Terminate();
    void Write(const void *buf, const size_t &size, const size_t &num, const Long64_t &pos=-1) const;
    void WriteHeader() const;

  private:
    QOversizeArray(): fFirstDataByte(0){}
    QOversizeArray(const QOversizeArray &): fFirstDataByte(0){}
    const QOversizeArray& operator=(const QOversizeArray &){return *this;}

    TString fFilename;
    TString fArrayName;
    FILE *fPtr;                 //!
    const Long64_t fFirstDataByte;
    omode fOpenMode;
    UInt_t fObjectsSize;
    void *fBuffer;              //!
    UInt_t fNOPerBuffer;
    Long64_t fNObjects;
    Long64_t fArrayMaxMemSize;
    QOABuffer *fCurReadBuffer;   //!
    QOABuffer *fFirstReadBuffer; //!
    QOABuffer *fLastReadBuffer;  //!
    QOABuffer *fWriteBuffer;     //!
    UInt_t fNReadBuffers;
    QOABuffer *fFirstParkedBuffer; //!
    UInt_t fNParkedBuffers;        //!
    static UInt_t fNInstances;
    static Long64_t fAAMaxMemSize;

    ClassDef(QOversizeArray,1)
};

#endif
