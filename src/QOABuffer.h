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
	QOABuffer(const Long64_t &firstobjidx, const UInt_t &arraysize, QOABuffer *previousoab=NULL, QOABuffer *nextoab=NULL): fFirstObjIdx(firstobjidx), fBuffer((char*)malloc(arraysize)), fIsModified(kFALSE), fPreviousOAB(previousoab), fNextOAB(nextoab), fBufferMutex(PTHREAD_MUTEX_INITIALIZER) {
//	    printf("%p\tQOABuffer(const Long64_t &firstobjidx<%lli>, const UInt_t &arraysize<%u>, const QOABuffer *previousoab<%p>, QOABuffer *nextoab<%p>)\n",this,firstobjidx,arraysize,previousoab,nextoab);
	}
	virtual ~QOABuffer(){free(fBuffer);}

	friend class QOversizeArray;

    private:
	QOABuffer(const QOABuffer &rhs);
	const QOABuffer& operator=(const QOABuffer &rhs);

	Long64_t fFirstObjIdx;
	char *fBuffer;           //!
	Bool_t fIsModified;      //!
	QOABuffer *fPreviousOAB; //! Previous QOABuffer (list sorted according to fFirstObjIdx)
	QOABuffer *fNextOAB;     //! Next QOABuffer (list sorted according to fFirstObjIdx)
	pthread_mutex_t fBufferMutex; //Lock on the buffer elements, fFrstObjIdx and fIsModified
	ClassDef(QOABuffer,1)
};

#endif
