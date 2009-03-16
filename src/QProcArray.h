#ifndef _QPROCARRAY_
#define _QPROCARRAY_

#include "TString.h"
#include "QProcObj.h"

class QProcArray: public QProcObj
{
  public:
    enum TypeID {kBool, kChar, kUChar, kShort, kUShort, kInt, kUInt, kFloat, kDouble, kLong64, kULong64};
    QProcArray(): QProcObj(), fTypeID(-1) {}
    QProcArray(const QProcArray &rhs): QProcObj(rhs), fTypeID(-1) {}
    virtual ~QProcArray(){}
    const QProcArray& operator=(const QProcArray &rhs){QProcObj::operator=(rhs); return *this;}
    virtual Int_t Fill()=0;
    virtual void* GetBuffer() const=0;
    virtual Long64_t GetEntries() const=0;
    const Int_t& GetTypeID() const{return fTypeID;}
    static Int_t GetNameTypeID(const TString adesc, TString *name);
    static UInt_t GetTypeIDSize(const Int_t &type);
    virtual void LoadEntry(const Long64_t &entry = 0)=0;
    virtual void ResetArray()=0;
    virtual void SetBuffer(void* buffer)=0;
    virtual void UnloadArray()=0;

  protected:
    Int_t fTypeID;

  private:
    ClassDef(QProcArray,1) //Pure abstract base class for QArrayProcessor arrays
};

#endif
