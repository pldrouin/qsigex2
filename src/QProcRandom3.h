// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QPROCRANDOM3_
#define _QPROCRANDOM3_

#include "TRandom3.h"
#include "QProcObj.h"
#include "QList.h"

class QProcRandom3: public QProcObj, public TRandom3
{
  public:
    QProcRandom3(): QProcObj(), TRandom3(){SetInitSeed((UInt_t)(4294967294u*fSRndm.Rndm())+1); fSInstances.Add(this);}
    QProcRandom3(const QProcRandom3 &rhs): QProcObj(rhs), TRandom3(rhs){fSInstances.Add(this);}
    virtual ~QProcRandom3(){fSInstances.Del(this);}
    static UInt_t GetInitGSeed(){return fInitGSeed;}
    UInt_t GetInitSeed() const{return fInitSeed;}
    void InitProcObj(){SetSeed(fInitSeed);}
    static void SetGSeed(const UInt_t &seed=0);
    void SetInitSeed(UInt_t seed=0){fInitSeed=seed; SetSeed(seed);}
  protected:
    UInt_t fInitSeed;
    static UInt_t fInitGSeed;
    static TRandom fSRndm;
    static QList<QProcObj*> fSInstances;
    ClassDef(QProcRandom3,1) //TRandom3 with QProcObj properties
};

#endif
