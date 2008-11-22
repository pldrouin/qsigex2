#ifndef _QPROCRANDOM3_
#define _QPROCRANDOM3_

#include "TRandom3.h"
#include "QProcObj.h"

class QProcRandom3: public QProcObj, public TRandom3
{
  public:
    QProcRandom3(): QProcObj(), TRandom3(){SetSeed((UInt_t)(4294967294u*fSRndm.Rndm())+1);}
    QProcRandom3(const QProcRandom3 &rhs): QProcObj(rhs), TRandom3(rhs){}
    virtual ~QProcRandom3(){}
    void InitProcObj(){TRandom3::SetSeed(fSeed);}
    static void SetGSeed(UInt_t seed=0){fSRndm.SetSeed(seed);}
    void SetSeed(UInt_t seed=0){fSeed=seed; TRandom3::SetSeed(seed);}
  protected:
    UInt_t fSeed;
    static TRandom3 fSRndm;
    ClassDef(QProcRandom3,1) //TRandom3 with QProcObj properties
};

#endif
