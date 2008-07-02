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
	QOABuffer(const UInt_t &bufferidx, const UInt_t &arraysize, QOABuffer *previousoab=NULL, QOABuffer *nextoab=NULL): fBufferIdx(bufferidx), fBuffer((char*)malloc(arraysize)), fBufferSize(arraysize), fIsCompressed(0), fIsModified(kFALSE), fPreviousOAB(previousoab), fNextOAB(nextoab), fBufferMutex(PTHREAD_MUTEX_INITIALIZER) {
	    //printf("%p\tQOABuffer(const UInt_t &bufferidx<%u>, const UInt_t &arraysize<%u>, const QOABuffer *previousoab<%p>, QOABuffer *nextoab<%p>)\n",this,bufferidx,arraysize,previousoab,nextoab);
	}
	virtual ~QOABuffer(){free(fBuffer);}

	friend class QOversizeArray;

    private:
	QOABuffer(const QOABuffer &rhs);
	const QOABuffer& operator=(const QOABuffer &rhs);

	UInt_t fBufferIdx;
	char *fBuffer;           //!
	Int_t fBufferSize;       //!
	Char_t fIsCompressed;    //! 0:Not Compressed 1:Compressed 2:Being compressed
	Bool_t fIsModified;      //!
	QOABuffer *fPreviousOAB; //! Previous QOABuffer (list sorted according to fBufferIdx)
	QOABuffer *fNextOAB;     //! Next QOABuffer (list sorted according to fBufferIdx)
	pthread_mutex_t fBufferMutex; //Lock on the buffer elements, fFrstObjIdx and fIsModified
	ClassDef(QOABuffer,1)
};

#endif
