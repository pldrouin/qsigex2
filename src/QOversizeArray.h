#ifndef _OVERSIZEARRAY_
#define _OVERSIZEARRAY_

#include "Rtypes.h"

class QOversizeArray
{
  public:
    QOversizeArray(UInt_t objectsize, UInt_t nobjectsperbuffer, Long64_t arraymaxsize=-1, Long64_t allarraysmaxsize=-2): fObjectsSize(objectsize), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fNObjects(0), fArrayMaxSize(arraymaxsize) {fNInstances++; if(allarraysmaxsize != -2) fAAMaxSize=allarraysmaxsize;}
    QOversizeArray(const QOversizeArray &){}
    virtual ~QOversizeArray(){fNInstances--;}
    const QOversizeArray& operator=(const QOversizeArray &){return *this;}

    void* GetBuffer(){return fBuffer;}

    void SetBuffer(void *buffer){fBuffer=buffer;}
  private:
    QOversizeArray(){}


    UInt_t fObjectsSize;
    void *fBuffer;
    UInt_t fNOPerBuffer;
    Int_t fNObjects;
    Long64_t fArrayMaxSize;
    static UInt_t fNInstances;
    static Long64_t fAAMaxSize;

    ClassDef(QOversizeArray,1)
};

#endif
