#ifndef _QPROCRANDOM3_
#define _QPROCRANDOM3_

#include "TRandom3.h"
#include "QProcObj.h"
#include "QList.h"

class QProcRandom3: public QProcObj, public TRandom3
{
  public:
    QProcRandom3(): QProcObj(), TRandom3(){SetSeed((UInt_t)(4294967294u*fSRndm.Rndm())+1); fSInstances.Add(this);}
    QProcRandom3(const QProcRandom3 &rhs): QProcObj(rhs), TRandom3(rhs){fSInstances.Add(this);}
    virtual ~QProcRandom3(){fSInstances.Del(this);}
    static UInt_t GetInitGSeed(){return fInitGSeed;}
    UInt_t GetInitSeed() const{return fInitSeed;}
    void InitProcObj(){TRandom3::SetSeed(fInitSeed);}
    static void SetGSeed(UInt_t seed=0);
    void SetSeed(UInt_t seed=0){fInitSeed=seed; TRandom3::SetSeed(seed);}
  protected:
    UInt_t fInitSeed;
    static UInt_t fInitGSeed;
    static TRandom3 fSRndm;
    static QList<QProcObj*> fSInstances;
    ClassDef(QProcRandom3,1) //TRandom3 with QProcObj properties
};

#endif
