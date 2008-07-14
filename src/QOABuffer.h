#ifndef _QOABUFFER_
#define _QOABUFFER_

#include <cstdlib>
#ifndef __CINT__
#include <pthread.h>
#else
struct pthread_t;
struct pthread_mutex_t;
struct pthread_cond_t;
#endif
#include "Rtypes.h"

class QOABuffer
{
  public:
    QOABuffer(const UInt_t &bufferidx, Int_t buffersize, QOABuffer *previousoab=NULL, QOABuffer *nextoab=NULL): fBufferIdx(bufferidx), fBuffer((char*)malloc(buffersize)), fBufferSize(buffersize), fIsCompressed(0), fIsModified(kFALSE), fPreviousOAB(previousoab), fNextOAB(nextoab) {
      //printf("%p\tQOABuffer(const UInt_t &bufferidx<%u>, const UInt_t &arraysize<%u>, const QOABuffer *previousoab<%p>, QOABuffer *nextoab<%p>)\n",this,bufferidx,arraysize,previousoab,nextoab);
    }
    virtual ~QOABuffer(){free(fBuffer);}

    friend class QOversizeArray;

  private:
    QOABuffer(const QOABuffer &rhs);
    const QOABuffer& operator=(const QOABuffer &rhs);

    Int_t fBufferIdx;
    char *fBuffer;           //!
    Int_t fBufferSize;      //!
    Char_t fIsCompressed;    //! 0:Not Compressed 1:Compressed 2:Being compressed 3:Being uncompressed 4:Uncompressed. Note: A compressed buffer is not necessarily stored in fBuffer but an uncompressed buffer is
    Bool_t fIsModified;      //!
    QOABuffer *fPreviousOAB; //! Previous QOABuffer (list sorted according to fBufferIdx)
    QOABuffer *fNextOAB;     //! Next QOABuffer (list sorted according to fBufferIdx)
    ClassDef(QOABuffer,1)
};

#endif
