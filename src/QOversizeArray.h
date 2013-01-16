// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QOVERSIZEARRAY_
#define _QOVERSIZEARRAY_

#ifndef __CINT__
#include <pthread.h>
#include <semaphore.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/time.h>
#include <stdint.h>

#include "qatomic.h"
#include "sigcontrol.h"
#else
typedef UInt_t uint32_t;
typedef Int_t int32_t;
typedef Long64_t int64_t;
struct pthread_t;
struct pthread_mutex_t;
struct pthread_cond_t;
struct sem_t;
#endif
#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <algorithm>
#include <errno.h>

#include "Rtypes.h"
#include "TString.h"
#include "TRandom.h"
#include "TTimeStamp.h"

#include "strdiffer.h"

#ifdef WITH_LIBPROCINFO
#include "procinfo.h"
#endif

#include "QOABuffer.h"
#include "QList.h"

#define QOA_MAXPROCS 16

extern "C" void R__zip (Int_t cxlevel, Int_t *nin, char *bufin, Int_t *lout, char *bufout, Int_t *nout);
extern "C" void R__unzip(Int_t *nin, UChar_t *bufin, Int_t *lout, char *bufout, Int_t *nout);

class QOversizeArray;

struct QOAQueue
{
  QOversizeArray	*Array; //Protected by fBuffersMutex
  struct QOAQueue	*Next;  //Protected by fB2LMutex
};

class QOversizeArray
{
  public:
    enum omode{kRead, kRW, kRecreate};
    QOversizeArray(const char *filename, const char *arraydescr, const Int_t& openmode=kRead, const UInt_t &objectsize=0, const UInt_t &nobjectsperbuffer=0, const Int_t &npcbuffers=1, const UInt_t &nobjectsallocblock=0);
    virtual ~QOversizeArray();

    static void ClearShMem();

    void CloseFile();

    void Fill();

    void* GetBuffer() const{return fBuffer;}
    Long64_t GetEntries() const{return fNObjects;}

    const Char_t* GetArrayName() const{return fArrayName;}
    const UInt_t& GetObjSize() const{return fObjectSize;}
    const Char_t* GetObjTypeName() const{return fObjectTypeName.Data();}

    static void InitShMem();

    const Bool_t& IsReadThreadSafe() const{return kFALSE;}

    static void KillThreads();

    void LoadEntry(const Long64_t &entry = 0);

    Int_t GetOpenMode(){return fOpenMode;}

    const TTimeStamp& GetTimeStamp() const{return fTStamp;}

    void OpenFile();

    void PrintInfo() const;
    static void PrintPriorities();

    void ResetArray();

    static void ResetPriorities();

    void Save(const Float_t &compfrac=0);

    void SetBuffer(void *buffer){fBuffer=buffer;}

    static void SetMemConstraints(const Long64_t &minmemsize=0, Float_t critlevel=0, const Float_t &level1=0, const Float_t &level2=0, Float_t cthreshlevel=-1);

#ifdef WITH_LIBPROCINFO
    static void SetMemUpdateInt(const time_t &nsecs){fMemUpdateInt=nsecs;}
#endif

    static void SetNLoaders(const UInt_t &nloaders){fNLoaders=(nloaders>0?nloaders:1);}

    static void ShowMemStats();

    void SetNOAllocBlock(const UInt_t &noallocblock){fNOAllocBlock=noallocblock;}

    void UpdateTimeStamp(){fTStamp.Set();}

  protected:
    QOversizeArray(): fFirstDataByte(0), fBufferHeaderSize(0) {}
    static void CheckMemory(); //Can only be called by the main thread and the BL threads
    void CleanUZBuffers();
    void Init();
    void ReadHeader();
    void ReadBuffer(QOABuffer **buf, const UInt_t &bufferidx);
    void ReadWriteBuffer();
    void Terminate();
    static void UpdateMemStats();
    void WriteHeader() const;
    void WriteBuffer(const QOABuffer *buf) const;
    void WriteWriteBuffer() const;

  private:
    QOversizeArray(const QOversizeArray &): fFirstDataByte(0), fBufferHeaderSize(0) {}
    const QOversizeArray& operator=(const QOversizeArray &){return *this;}

    TString fFilename;
    TString fArrayName;
    TTimeStamp fTStamp;          // Timestamp associated with the array
    int fFDesc;                  //!
    const UInt_t fFirstDataByte; // Size of file header
    const Int_t fBufferHeaderSize; // Size of buffer header
    Int_t fOpenMode;             // Array opening mode
    UInt_t fObjectSize;          // Size of a single stored object
    TString fObjectTypeName;
    void *fBuffer;               //! Pointer to the current object
    UInt_t fNOPerBuffer;         // Maximum number of objects per buffer
    UInt_t fNOAllocBlock;        // Number of objects for which memory is allocated at once in the write buffer
    Int_t fMaxBDataSize;         //Maximum buffer data size
    Int_t fMaxBHBDataSize;       //fBufferHeaderSize+fMaxBHBDataSize 
    Long64_t fNObjects;          // Total number of objects
    QOABuffer *fCurReadBuffer;   //! Current read buffer. Should always point to a buffer with fBufferIdx<=fCurRBIdx
    QOABuffer *fFirstReadBuffer; //!
    QOABuffer *fLastReadBuffer;  //!
    QOABuffer *fWriteBuffer;     //!
    Char_t    *fCRBData;         //! Pointer to current read buffer uncompressed data
    Long64_t   fWBFirstObjIdx;   // Index of the first object contained in the write buffer. ****Value should be modified only by the main thread
    UInt_t     fWBNAllocObjs;    // Number of objects for which memory is allocated in the write buffer
    Int_t fCurRBIdx;             // Current read buffer index. A value of -1 indicates the array is in write mode, a value of -2 indicates that unzipped buffers should be cleaned by QOABLThread. A value >=0 indicates that the main thread considers the array to be in read mode. Read mode check should also ensure that fCurBLRBIdx>=0.
    Int_t fCurBLRBIdx;           // Current read buffer index used by buffer loading thread. Value should not be changed outside of QOABLThread unless it is not working on this. A value of -2 indicates that QOABLThread does not consider the array to be in read mode.
    Int_t fNReadBuffers;         // Number of active buffers that are full and ready for reading
    Long64_t fPTNRBObjects;      // Total number of objects in read buffers in the previous pass (==fWBFirstObjIdx after the last call of Fill())
    QOABuffer **fUMBuffers;       //! Array of unmodified buffers
    Int_t fNUMBuffers;           // Number of unmodified buffers
    QOABuffer *fFirstParkedBuffer;//!
    Int_t fNPCBuffers;           // Number of buffers that are pre-cached (to speed up reading)
    Long64_t fArrayMemSize;      // Total size used by array buffers (including size used by QOABuffer member variables and by zipped/unzipped buffers).
    Float_t fArrayIO;            // Total IO for this array (reading+writing)
    Float_t fAPriority;          // Priority for this array (1/fArrayIO)
    mutable pthread_mutex_t fFileMutex; // Mutex on file reading & writing operations
    pthread_mutex_t fBuffersMutex; // Lock on all buffer linked list structure, excluding parked buffers (all "read" QOABuffer pointers + counters, fWBFirstObjIdx, fCurRBIdx, QOABuffer::fIsModified, QOABuffer::fIsCompressed)
    pthread_mutex_t fPBuffersMutex; //Lock on all parked buffer linked list structure
    pthread_t fMWThread;           // Memory writing thread
    pthread_mutex_t fMWMutex;      // Memory writing thread mutex
    pthread_cond_t fMWCond;        // Memory writing thread condition
    pthread_mutex_t fMWCMutex;     // Memory writing thread condition mutex
    pthread_cond_t fMWPCond;       // Memory writing thread pausing condition
    pthread_cond_t fMWCCond;       // Memory writing thread confirmation condition
    Char_t          fMWAction;     // Flag to control the action of the memory writing thread (0: Normal 1: Pause 2: Stop)
    QOABuffer      *fMWBuffer;     // QOABuffer to be freed by fMWThread
    QOABuffer      *fMWWBuffer;    // QOABuffer being written by fMWThread
    Bool_t	   fSigMMThread;   // Send signal to memory management thread
    sem_t	   fBLCSem;        // Buffer loading thread confirmation semaphore
    sem_t	   fBLWSem;        // Buffer loading thread waiting condition semaphore
    Char_t          fBLAction;     // Flag to control the action of the buffer loading thread (0: Normal 1: Wait 2: Stop)
    struct QOAQueue	   fQOAQ;          // QOAQueue structure for this
    QOABuffer	   *fCurBLBuffer;  //Current buffer hold by QOABLThread
    QOABuffer     **fUZQOAB;       //! Array of QOABuffers that have been unzipped
    Char_t        **fUZBuffers;    //! Array of unzipped buffers
    pthread_mutex_t fUZBMutex;     // Lock on unzipped buffer arrays

    static QList<QOversizeArray*> fInstances; // List of QOversizeArray instances
    static const unsigned int sSHPAMSize; //Shared Header Page-Aligned memory size
    static int   fShFDesc;         //!
    static bool  fShOwns;             //!
#pragma pack(push)  /* push current alignment to stack */
#pragma pack(1)     /* set alignment to 1 byte boundary */
    static struct sharst {
      int32_t susers;
      bool used[QOA_MAXPROCS];
      Long64_t memory[QOA_MAXPROCS];
    } *fShArSt;
#pragma pack(pop)   /* restore original alignment from stack */
    static QList<Float_t>         fICumulPriority; // Cumulative array priorities
    static Long64_t fMinMemSize;    // Minimum amount of memory needed for all QOversizeArray instances of the current process
    static Long64_t fLevel1MemSize; // Memory level (B) at which memory management thread stops attempting to free memory
    static Float_t  fLevel1;        // Memory level (fraction) at which memory management thread stops attempting to free memory
    static Long64_t fLevel2MemSize; // Memory level (B) at which memory management thread starts attempting to free memory
    static Float_t  fLevel2;        // Memory level (fraction) at which memory management thread starts attempting to free memory
    static Long64_t fCritMemSize;   // Critical memory level (B) at which the main thread pauses until some memory is freed
    static Float_t  fCritLevel;   // Critical memory level (fraction) at which the main thread pauses until some memory is freed
    static Long64_t fCThreshMemSize; // Memory level (B) at which memory management thread starts compressing the buffers to save memory
    static Float_t  fCThreshLevel;   // Memory level (fraction) at which memory management thread starts compressing the buffers to save memory
    static Long64_t *fTotalMemSize;  //Total amount of memory used by the buffers of all arrays
#ifdef WITH_LIBPROCINFO
    static time_t   fMemUpdateInt; //Memory usage update time interval
    static time_t   fMemUpdateTime; //Memory usage update time interval
#endif
    static UInt_t   fNLoaders;      //Number of QOABLThreads
    //static pthread_mutex_t fMSizeMutex; //Total memory size mutex
    static pthread_mutex_t fCMSCMutex;    //Critical memory size mutex
    static pthread_cond_t   fCMSCond;     //Critical memory size condition
    static Bool_t fCLReached;           //Indicate when critical memory size has been reached
    static pthread_t fMMThread;      //Memory management thread
    static pthread_mutex_t fMMMutex; //Memory management condition mutex
    static pthread_cond_t fMMCond;   //Memory management condition
    static pthread_mutex_t fFileWMutex; // Static mutex on file writing operations (patch for stability while waiting for a FIFO queue implementation)
    static pthread_mutex_t fILMutex; //Instance list mutex. Locks the existence of a QOversizeArray instance.
    static pthread_mutex_t fPriorityMutex; //Mutex for instance priorities
    static pthread_mutex_t fMWCDMutex;    // Memory writing/compression thread done condition mutex
    static pthread_cond_t fMWCDCond;      // Memory writing/compression thread done confirmation condition
    static pthread_t     *fBLThreads;     // Buffer loading threads
    static pthread_t fMCThread;           // Memory compression thread
    static pthread_mutex_t fMCGMutex;     // Memory compression thread giant mutex
    static pthread_mutex_t fMCMutex;      // Memory compression thread mutex
    static pthread_cond_t fMCCond;        // Memory compression thread condition
    static pthread_mutex_t fMCCMutex;     // Memory compression thread condition mutex
    static pthread_cond_t fMCPCond;       // Memory compression thread pausing condition
    static pthread_cond_t fMCCCond;       // Memory compression thread confirmation condition
    static pthread_mutex_t fMCDMutex;    // Memory compression thread done condition mutex
    static pthread_cond_t fMCDCond;      // Memory compression thread done confirmation condition
    static Char_t         fMCAction;      // Flag to control the action of the memory compression thread (0: Normal 1: Pause 2: Stop)
    static QOversizeArray *fMCQOA;        // QOversizeArray to be compressed by fMCThread
    static QOABuffer      *fMCBuffer;     // QOABuffer to be compressed by fMCThread
    static QOABuffer      *fMCCBuffer;    // QOABuffer being compressed by fMCThread
    static Int_t	fMaxNPCBuffers;	  // Maximum number of buffers that are pre-cached (to speed up reading) 
    static struct QOAQueue	  *fFirstB2L;     // Head of the queue of buffers to load
    static struct QOAQueue      **fLastB2Ls;     // Tails of the queue of buffers to load (fNPCBuffers+1) pointers
    static pthread_mutex_t fB2LMutex;     // Buffers to load mutex
    static sem_t           fB2LSem;       // Buffers to load semaphore

    static void* QOAMCThread(void *array);
    static void* QOAMWThread(void *array);
    static void* QOABLThread(void*);
    static void* QOAMMThread(void *);

    ClassDef(QOversizeArray,1) //Multi-threaded array class optimized for speed when handling large amount of data
};

#endif
