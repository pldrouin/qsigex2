#ifndef _OVERSIZEARRAY_
#define _OVERSIZEARRAY_

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
#include <cstdlib>
#include <cstdio>
#include <cmath>
#include <algorithm>
#include <errno.h>

#include "Rtypes.h"
#include "TString.h"
#include "TRandom.h"
#include "TTimeStamp.h"
#include "QOABuffer.h"
#include "QList.h"

extern "C" void R__zip (Int_t cxlevel, Int_t *nin, char *bufin, Int_t *lout, char *bufout, Int_t *nout);
extern "C" void R__unzip(Int_t *nin, UChar_t *bufin, Int_t *lout, char *bufout, Int_t *nout);

class QOversizeArray
{
  public:
    enum omode{kRead, kRW, kRecreate};
    QOversizeArray(const char *filename, const char *arraydescr, omode openmode=kRead, const UInt_t &objectsize=0, const UInt_t &nobjectsperbuffer=0, const Int_t &npcbuffers=1, const UInt_t &nobjectsallocblock=0);
    virtual ~QOversizeArray();

    void CloseFile();

    void Fill();

    void* GetBuffer() const{return fBuffer;}
    Long64_t GetEntries() const{return fNObjects;}

    const Char_t* GetArrayName() const{return fArrayName;}
    const UInt_t& GetObjSize() const{return fObjectSize;}
    const Char_t* GetObjTypeName() const{return fObjectTypeName.Data();}

    void LoadEntry(const Long64_t &entry = 0);

    const omode& GetOpenMode(){return fOpenMode;}

    const TTimeStamp& GetTimeStamp() const{return fTStamp;}

    void OpenFile();

    void PrintInfo() const;
    static void PrintPriorities();

    void ResetArray();

    static void ResetPriorities();

    void Save(const Float_t &compfrac=0);

    void SetBuffer(void *buffer){fBuffer=buffer;}

    static void SetMemConstraints(const Long64_t &critmemsize=0, const Long64_t &level1memsize=0, const Long64_t &level2memsize=0, const Long64_t &cthreshmemsize=-1);

    void SetNOAllocBlock(const UInt_t &noallocblock){fNOAllocBlock=noallocblock;}

    void UpdateTimeStamp(){fTStamp.Set();}

  protected:
    QOversizeArray(): fFirstDataByte(0), fBufferHeaderSize(0) {}
    static void CheckMemory();
    void CleanUZBuffers();
    void Init();
    void ReadHeader();
    void ReadBuffer(QOABuffer **buf, const UInt_t &bufferidx);
    void ReadWriteBuffer();
    void Terminate();
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
    omode fOpenMode;             // Array opening mode
    UInt_t fObjectSize;          // Size of a single stored object
    TString fObjectTypeName;
    void *fBuffer;               //! Pointer to the current object
    UInt_t fNOPerBuffer;         // Maximum number of objects per buffer
    UInt_t fNOAllocBlock;        // Number of objects for which memory is allocated at once in the write buffer
    Int_t fMaxBDataSize;         //Maximum buffer data size
    Int_t fMaxBHBDataSize;       //fBufferHeaderSize+fMaxBHBDataSize 
    Long64_t fNObjects;          // Total number of objects
    QOABuffer *fCurReadBuffer;   //! Current read buffer
    QOABuffer *fFirstReadBuffer; //!
    QOABuffer *fLastReadBuffer;  //!
    QOABuffer *fWriteBuffer;     //!
    Char_t    *fCRBData;         //! Pointer to current read buffer uncompressed data
    Long64_t   fWBFirstObjIdx;   // Index of the first object contained in the write buffer. ****Value should be modified only by the main thread
    UInt_t     fWBNAllocObjs;    // Number of objects for which memory is allocated in the write buffer
    Int_t fCurRBIdx;             // Current read buffer index. A value of -1 indicates the array is in write mode
    Int_t fCurBLRBIdx;           // Current read buffer index used by buffer loading thread.
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
    pthread_t fBLThread;           // Buffer loading thread
    pthread_mutex_t fBLMutex;      // Buffer loading thread mutex
    pthread_cond_t fBLCond;        // Buffer loading thread condition
    pthread_mutex_t fBLCMutex;     // Buffer loading thread condition mutex
    pthread_cond_t fBLCCond;       // Buffer loading thread confirmation condition
    pthread_cond_t fBLWCond;       // Buffer loading thread waiting condition. Can use this instead of a pause condition for this thread since only the main thread can be calling for both a waiting condition or a command
    Char_t          fBLAction;     // Flag to control the action of the buffer loading thread (0: Normal 1: Wait 2: Stop)
    QOABuffer     **fUZQOAB;       //! Array of QOABuffers that have been unzipped
    Char_t        **fUZBuffers;    //! Array of unzipped buffers
    pthread_mutex_t fUZBMutex;     // Lock on unzipped buffer arrays
    static QList<QOversizeArray*> fInstances; // List of QOversizeArray instances
    static QList<Float_t>         fICumulPriority; // Cumulative array priorities
    static Long64_t fLevel1MemSize; // Memory level at which memory management thread stops attempting to free memory
    static Long64_t fLevel2MemSize; // Memory level at which memory management thread starts attempting to free memory
    static Long64_t fCritMemSize;   // Critical memory level at which the main thread pauses until some memory is freed
    static Long64_t fCThreshMemSize; // Memory level at which memory management thread starts compressing the buffers to save memory
    static Long64_t fTotalMemSize;  //Total amount of memory used by the buffers of all arrays
    static pthread_mutex_t fMSizeMutex; //Total memory size mutex
    static pthread_mutex_t fCMSCMutex;  //Critical memory size condition mutex
    static pthread_cond_t fCMSCond;     //Critical memory size condition
    static Bool_t fCLReached;           //Indicate when critical memory size has been reached
    static pthread_t fMMThread;      //Memory management thread
    static pthread_mutex_t fMMMutex; //Memory management condition mutex
    static pthread_cond_t fMMCond;   //Memory management condition
    static pthread_mutex_t fILMutex; //Instance list mutex. Locks the existence of a QOversizeArray instance.
    static pthread_mutex_t fPriorityMutex; //Mutex for instance priorities
    static pthread_mutex_t fMWCDMutex;    // Memory writing/compression thread done condition mutex
    static pthread_cond_t fMWCDCond;      // Memory writing/compression thread done confirmation condition
    static pthread_t fMCThread;           // Memory compression thread
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

    static void* QOAMCThread(void *array);
    static void* QOAMWThread(void *array);
    static void* QOABLThread(void *array);
    static void* QOAMMThread(void *);

    ClassDef(QOversizeArray,1) //Multi-threaded array class optimized for speed when handling large amount of data
};

#endif
