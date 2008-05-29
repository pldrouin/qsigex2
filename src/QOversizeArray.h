#ifndef _OVERSIZEARRAY_
#define _OVERSIZEARRAY_

#include <cstdio>
#include <cstring>
#include "Rtypes.h"
#include "TString.h"

class QOversizeArray
{
  public:
    enum omode{kRead, kRW, kRecreate};
    QOversizeArray(const char *filename, const char *arrayname, omode openmode=kRead, UInt_t objectsize=0, UInt_t nobjectsperbuffer=1024, Long64_t arraymaxsize=-1, Long64_t allarraysmaxsize=-2);
    virtual ~QOversizeArray();

    void CloseFile();

    void* GetBuffer(){return fBuffer;}
    Long64_t GetNObjects(){return fNObjects;}

    void OpenFile();

    void SetBuffer(void *buffer){fBuffer=buffer;}
  protected:
    void Read(void *buf, size_t size, size_t num, Long64_t pos=-1);
    void ReadHeader();
    void Write(const void *buf, size_t size, size_t num, Long64_t pos=-1) const;
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
    Long64_t fArrayMaxSize;
    static UInt_t fNInstances;
    static Long64_t fAAMaxSize;

    ClassDef(QOversizeArray,1)
};

#endif
