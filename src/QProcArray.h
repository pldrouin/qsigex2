#ifndef _QPROCARRAY_
#define _QPROCARRAY_

#include "QProcObj.h"

class QProcArray: public QProcObj
{
  public:
    QProcArray(): QProcObj() {}
    QProcArray(const QProcArray &rhs): QProcObj(rhs) {}
    virtual ~QProcArray(){}
    const QProcArray& operator=(const QProcArray &rhs){QProcObj::operator=(rhs); return *this;}
    virtual Int_t Fill()=0;
    virtual void* GetBuffer() const=0;
    virtual Long64_t GetEntries() const=0;
    virtual Int_t GetEntry(Long64_t entry = 0, Int_t dummy=0)=0;
    virtual void ResetArray()=0;
    virtual void SetBuffer(void* buffer)=0;
    virtual void UnloadArray()=0;
  protected:
  private:
    ClassDef(QProcArray,1) //Pure abstract base class for QArrayProcessor arrays
};

#endif
