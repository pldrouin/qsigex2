#ifndef _OVERSIZEARRAY_
#define _OVERSIZEARRAY_

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <errno.h>
#ifndef __CINT__
#include <pthread.h>
#else
struct pthread_t;
struct pthread_mutex_t;
struct pthread_cond_t;
#endif
#include <signal.h>
#include "Rtypes.h"
#include "TString.h"
#include "QOABuffer.h"
#include "QList.h"

class QOversizeArray
{
  public:
    enum omode{kRead, kRW, kRecreate};
    QOversizeArray(const char *filename, const char *arrayname, omode openmode=kRead, const UInt_t &objectsize=0, const UInt_t &nobjectsperbuffer=131072);
    virtual ~QOversizeArray();

    void CloseFile();

    void Fill();

    void* GetBuffer() const{return fBuffer;}
    Long64_t GetNObjects() const{return fNObjects;}

    void OpenFile();

    static void ResetPriorities();

    void Save();

    void SetBuffer(void *buffer){fBuffer=buffer;}

    void SetBufferCaching(UInt_t nicbuffers=2, UInt_t npcbuffers=3){fNICBuffers=nicbuffers; fNPCBuffers=npcbuffers;}

    static void SetMemConstraints(const Long64_t &critmemsize=0, const Long64_t &level1memsize=0, const Long64_t &level2memsize=0);

  protected:
    void CheckMemory();
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
    void *fBuffer;               //!
    UInt_t fNOPerBuffer;
    Long64_t fNObjects;
    QOABuffer *fCurReadBuffer;   //!
    QOABuffer *fFirstReadBuffer; //!
    QOABuffer *fLastReadBuffer;  //!
    QOABuffer *fWriteBuffer;     //!
    UInt_t fCurReadBufferIdx;    //! Index of current read buffer
    UInt_t fNReadBuffers;
    QOABuffer **fUMBuffers;       //! Array of unmodified buffers
    UInt_t fNUMBuffers;           // Number of unmodified buffers
    QOABuffer *fFirstParkedBuffer;//!
    UInt_t fNICBuffers;           // Number of buffers that are initially cached (to speed up reading of first entries)
    UInt_t fNPCBuffers;           // Number of buffers that are pre-cached (to speed up reading)
    Float_t fArrayIO;
    Float_t fAPriority;
    pthread_mutex_t fFileMutex;
    static QList<QOversizeArray*> fInstances;
    static Long64_t fLevel1MemSize;
    static Long64_t fLevel2MemSize;
    static Long64_t fCritMemSize;
    static Long64_t fTotalMemSize;
    static pthread_mutex_t fMSizeMutex;
    static pthread_t fMMThread;
    static pthread_mutex_t fMMMutex;
    static pthread_cond_t fMMCond;
    static pthread_mutex_t fILMutex; //Instance list mutex

    static void* QOAReadThread(void *ptr);
    static void* QOAMMThread(void *ptr);

    ClassDef(QOversizeArray,1)
};

#endif
