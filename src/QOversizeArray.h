#ifndef _OVERSIZEARRAY_
#define _OVERSIZEARRAY_

#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <errno.h>
#ifndef __CINT__
#include <pthread.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
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

extern "C" void R__zip (Int_t cxlevel, Int_t *nin, char *bufin, Int_t *lout, char *bufout, Int_t *nout);
extern "C" void R__unzip(Int_t *nin, UChar_t *bufin, Int_t *lout, char *bufout, Int_t *nout);

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

    static void SetMemConstraints(const Long64_t &critmemsize=0, const Long64_t &level1memsize=0, const Long64_t &level2memsize=0, const Long64_t &cthreshmemsize=-1);

  protected:
    void CheckMemory();
    void Init();
    void ReadHeader();
    void ReadBuffer(QOABuffer **buf, const UInt_t &bufferidx);
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
    int fFDesc;                  //!
    const UInt_t fFirstDataByte;
    const Int_t fBufferHeaderSize;
    omode fOpenMode;
    UInt_t fObjectSize;
    void *fBuffer;               //!
    UInt_t fNOPerBuffer;
    Int_t fMaxBDataSize;         //Maximum buffer data size
    Int_t fMaxBHBDataSize;       //fBufferHeaderSize+fMaxBHBDataSize 
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
    pthread_mutex_t fBuffersMutex; // Lock on all buffer linked list structure (all "read" QOABuffer pointers + counters, QOABuffer::fIsModified, QOABuffer::fIsCompressed)
    pthread_mutex_t fRBDIMutex;    // Lock on read buffer data integrity (QOABuffer::fBuffer and QOABuffer::fBufferSize), for buffers IN THE LINKED LIST
    pthread_t fMWThread;           // Memory writing thread
    pthread_mutex_t fMWMutex;      // Memory writing thread mutex
    pthread_cond_t fMWCond;        // Memory writing thread condition
    pthread_mutex_t fMWCMutex;     // Memory writing thread condition mutex
    pthread_cond_t fMWPCond;       // Memory writing thread pausing condition
    pthread_cond_t fMWCCond;       // Memory writing thread confirmation condition
    Char_t          fMWAction;     // Flag to control the action of the memory writing thread (0: Normal 1: Pause 2: Stop)
    QOABuffer      *fMWBuffer;     // QOABuffer to be freed by fMWThread
    QOABuffer      *fMWWBuffer;    // QOABuffer being written by fMWThread
    static QList<QOversizeArray*> fInstances;
    static QList<Float_t>         fICumulPriority;
    static Long64_t fLevel1MemSize;
    static Long64_t fLevel2MemSize;
    static Long64_t fCritMemSize;
    static Long64_t fCThreshMemSize;
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

    static void* QOAMWThread(void *array);
    static void* QOAReadThread(void *array);
    static void* QOAMMThread(void *);

    ClassDef(QOversizeArray,1)
};

#endif
