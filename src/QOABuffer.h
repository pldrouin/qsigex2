#ifndef _QOABUFFER_
#define _QOABUFFER_

#include <cstdlib>
#include "Rtypes.h"

class QOABuffer
{
    public:
	QOABuffer(const Long64_t &firstobjidx, const UInt_t &arraysize, QOABuffer *previousoab=NULL, QOABuffer *nextoab=NULL): fFirstObjIdx(firstobjidx), fBuffer((char*)malloc(arraysize)), fIsModified(kFALSE), fPreviousOAB(previousoab), fNextOAB(nextoab) {
	    printf("%p\tQOABuffer(const Long64_t &firstobjidx<%lli>, const UInt_t &arraysize<%u>, const QOABuffer *previousoab<%p>, QOABuffer *nextoab<%p>)\n",this,firstobjidx,arraysize,previousoab,nextoab);
	}
	virtual ~QOABuffer(){free(fBuffer);}

	char* GetBuffer() const{return fBuffer;}
	const Long64_t& GetFirstObjIdx() const{return fFirstObjIdx;}
	QOABuffer* GetNextOAB() const{return fNextOAB;}
	QOABuffer* GetPreviousOAB() const{return fPreviousOAB;}
	Bool_t IsModified() const{return fIsModified;}

	void SetFirstObjIdx(const Long64_t &firstobjidx){fFirstObjIdx=firstobjidx;}
	void SetModified(Bool_t ismodified=kTRUE){fIsModified=ismodified;}
	void SetNextOAB(QOABuffer *nextoab){fNextOAB=nextoab;}
	void SetPreviousOAB(QOABuffer *previousoab){fPreviousOAB=previousoab;}

    private:
	QOABuffer(const QOABuffer &rhs);
	const QOABuffer& operator=(const QOABuffer &rhs);

	Long64_t fFirstObjIdx;
	char *fBuffer;           //!
	Bool_t fIsModified;      //!
	QOABuffer *fPreviousOAB; //! Previous QOABuffer (list sorted according to fFirstObjIdx)
	QOABuffer *fNextOAB;     //! Next QOABuffer (list sorted according to fFirstObjIdx)
	ClassDef(QOABuffer,1)
};

#endif
