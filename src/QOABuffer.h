#ifndef _QOABUFFER_
#define _QOABUFFER_

#include <cstdlib>
#include "Rtypes.h"

class QOABuffer
{
    public:
	QOABuffer(const Long64_t &firstobjidx, const UInt_t &arraysize, const QOABuffer *previousoab=NULL, const QOABuffer *nextoab=NULL): fFirstObjIdx(firstobjidx), fBuffer((char*)malloc(arraysize)), fPreviousOAB(previousoab), fNextOAB(nextoab), fIsModified(kFALSE), fIsLocked(kFALSE) {}
	QOABuffer(const QOABuffer &rhs);
	virtual ~QOABuffer(){free(fBuffer);}

	char* GetBuffer() const{return fBuffer;}
	const Long64_t& GetFirstObjIdx() const{return fFirstObjIdx;}
	const QOABuffer* GetNextOAB() const{return fNextOAB;}
	const QOABuffer* GetPreviousOAB() const{return fPreviousOAB;}

	Bool_t IsLocked() const{return fIsLocked;}
	Bool_t IsModified() const{return fIsModified;}

	const QOABuffer& operator=(const QOABuffer &rhs);

	void SetFirstObjIdx(const Long64_t &firstobjidx){fFirstObjIdx=firstobjidx;}
	void SetLocked(Bool_t locked=kTRUE){fIsLocked=locked;}
	void SetModified(Bool_t modified=kTRUE){fIsModified=modified;}
	void SetNextOAB(const QOABuffer *nextoab){fNextOAB=nextoab;}
	void SetPreviousOAB(const QOABuffer *previousoab){fPreviousOAB=previousoab;}

    private:
	Long64_t fFirstObjIdx;
	char *fBuffer;                 //!
	const QOABuffer *fPreviousOAB; //!
	const QOABuffer *fNextOAB;     //!
	Bool_t fIsModified;
	Bool_t fIsLocked;
	ClassDef(QOABuffer,1)
};

#endif
