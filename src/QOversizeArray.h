#ifndef _OVERSIZEARRAY_
#define _OVERSIZEARRAY_

#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <errno.h>
#ifndef __CINT__
#include <pthread.h>
#else
struct pthread_t;
struct pthread_mutex_t;
struct pthread_cond_t;
#endif
#include "Rtypes.h"
#include "TString.h"
#include "TRandom.h"
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

    void SetBufferCaching(UInt_t npcbuffers=3){pthread_mutex_lock(&fBuffersMutex); fNPCBuffers=npcbuffers; pthread_mutex_unlock(&fBuffersMutex);}

    static void SetMemConstraints(const Long64_t &critmemsize=0, const Long64_t &level1memsize=0, const Long64_t &level2memsize=0);

  protected:
    void CheckMemory();
    void Init();
    void ReadHeader();
    void ReadBuffer(QOABuffer *buf, const UInt_t &bufferidx);
    void ReadWriteBuffer();
    void Terminate();
    void WriteHeader() const;
    void WriteBuffer(const QOABuffer *buf) const;
    void WriteWriteBuffer() const;

  private:
    QOversizeArray(): fFirstDataByte(0), fBufferHeaderSize(0) {}
    QOversizeArray(const QOversizeArray &): fFirstDataByte(0), fBufferHeaderSize(0) {}
    const QOversizeArray& operator=(const QOversizeArray &){return *this;}

    TString fFilename;
    TString fArrayName;
    FILE *fPtr;                 //!
    const UInt_t fFirstDataByte;
    const UInt_t fBufferHeaderSize;
    omode fOpenMode;
    UInt_t fObjectSize;
    void *fBuffer;               //!
    UInt_t fNOPerBuffer;
    UInt_t fMaxBDataSize;        //Maximum buffer data size
    UInt_t fMaxBHBDataSize;      //fBufferHeaderSize+fMaxBHBDataSize 
    Long64_t fNObjects;
    QOABuffer *fCurReadBuffer;   //! Current read buffer. A NULL pointer means that the array is in writing mode
    QOABuffer *fFirstReadBuffer; //!
    QOABuffer *fLastReadBuffer;  //!
    QOABuffer *fWriteBuffer;     //!
    Long64_t   fWBFirstObjIdx;   // Index of the first object contained in the write buffer
    UInt_t fNReadBuffers;        // Number of active buffers that are full and ready for reading
    QOABuffer **fUMBuffers;       //! Array of unmodified buffers
    UInt_t fNUMBuffers;           // Number of unmodified buffers
    QOABuffer *fFirstParkedBuffer;//!
    UInt_t fNPCBuffers;           // Number of buffers that are pre-cached (to speed up reading)
    Float_t fArrayIO;
    Float_t fAPriority;
    mutable pthread_mutex_t fFileMutex;
    pthread_mutex_t fBuffersMutex; // Lock on all buffer linked list structure (all "read" QOABuffer pointers + counters)
    pthread_t fMFThread;           // Memory freeing thread
    pthread_mutex_t fMFMutex;      // Memory freeing thread mutex
    pthread_cond_t fMFCond;        // Memory freeing thread condition
    pthread_mutex_t fMFCMutex;     // Memory freeing thread condition mutex
    pthread_cond_t fMFPCond;       // Memory freeing thread pausing condition
    pthread_cond_t fMFCCond;       // Memory freeing thread confirmation condition
    Char_t          fMFAction;     // Flag to control the action of the memory freeing thread (0: Normal 1: Pause 2: Stop)
    QOABuffer      *fMFBuffer;     // QOABuffer to be freed by fMFThread.
    static QList<QOversizeArray*> fInstances;
    static QList<Float_t>         fICumulPriority;
    static Long64_t fLevel1MemSize;
    static Long64_t fLevel2MemSize;
    static Long64_t fCritMemSize;
    static Long64_t fTotalMemSize;
    static pthread_mutex_t fMSizeMutex; //Total memory size mutex
    static pthread_mutex_t fCMSCMutex;  //Critical memory size condition mutex
    static pthread_cond_t fCMSCond;     //Critical memory size condition
    static Bool_t fCLReached;           //Indicate when critical memory size has been reached
    static pthread_t fMMThread;      //Memory management thread
    static pthread_mutex_t fMMMutex; //Memory management condition mutex
    static pthread_cond_t fMMCond;   //Memory management condition
    static pthread_mutex_t fILMutex; //Instance list mutex. Locks the existence of a QOversizeArray instance.
    static pthread_mutex_t fPriorityMutex; //Mutex for instance priorities

    static void* QOAMFThread(void *array);
    static void* QOAReadThread(void *array);
    static void* QOAMMThread(void *);

    ClassDef(QOversizeArray,1)
};

#endif
