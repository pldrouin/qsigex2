#include "QProcRandom3.h"

ClassImp(QProcRandom3)

TRandom3 QProcRandom3::fSRndm(4357);
QList<QProcObj*> QProcRandom3::fSInstances;

void QProcRandom3::SetGSeed(UInt_t seed)
{
  fSRndm.SetSeed(seed);

  for(Int_t i=0; i<fSInstances.Count(); i++) {
    ((QProcRandom3*)fSInstances[i])->SetSeed((UInt_t)(4294967294u*fSRndm.Rndm())+1);
  }
}
