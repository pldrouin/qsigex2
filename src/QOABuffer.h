#ifndef _QOABUFFER_
#define _QOABUFFER_

#include <cstdlib>
#include "Rtypes.h"

class QOABuffer
{
    public:
	QOABuffer(const Long64_t &firstobjidx, const UInt_t &arraysize, QOABuffer *previousoab=NULL, QOABuffer *nextoab=NULL): fFirstObjIdx(firstobjidx), fBuffer((char*)malloc(arraysize)), fPreviousOAB(previousoab), fNextOAB(nextoab) {
	    printf("%p\tQOABuffer(const Long64_t &firstobjidx<%lli>, const UInt_t &arraysize<%u>, const QOABuffer *previousoab<%p>, QOABuffer *nextoab<%p>)\n",this,firstobjidx,arraysize,previousoab,nextoab);
	}
	virtual ~QOABuffer(){free(fBuffer);}

	char* GetBuffer() const{return fBuffer;}
	const Long64_t& GetFirstObjIdx() const{return fFirstObjIdx;}
	QOABuffer* GetNextOAB() const{return fNextOAB;}
	QOABuffer* GetPreviousOAB() const{return fPreviousOAB;}

	void SetFirstObjIdx(const Long64_t &firstobjidx){fFirstObjIdx=firstobjidx;}
	void SetNextOAB(QOABuffer *nextoab){fNextOAB=nextoab;}
	void SetPreviousOAB(QOABuffer *previousoab){fPreviousOAB=previousoab;}

    private:
	QOABuffer(const QOABuffer &rhs);
	const QOABuffer& operator=(const QOABuffer &rhs);

	Long64_t fFirstObjIdx;
	char *fBuffer;           //!
	QOABuffer *fPreviousOAB; //!
	QOABuffer *fNextOAB;     //!
	ClassDef(QOABuffer,1)
};

#endif
