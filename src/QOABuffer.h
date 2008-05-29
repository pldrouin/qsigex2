#ifndef _QOABUFFER_
#define _QOABUFFER_

#include "Rtypes.h"

class QOABuffer
{
    public:
	QOABuffer(){}
	QOABuffer(const QOABuffer &){}
	virtual ~QOABuffer(){}
	const QOABuffer& operator=(const QOABuffer &){return *this;}
    private:
	ClassDef(QOABuffer,1)
};

#endif
