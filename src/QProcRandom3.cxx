// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#include "QProcRandom3.h"

ClassImp(QProcRandom3)

UInt_t QProcRandom3::fInitGSeed(4357);
TRandom QProcRandom3::fSRndm(4357);
QList<QProcObj*> QProcRandom3::fSInstances;

void QProcRandom3::SetGSeed(const UInt_t &seed)
{
  fInitGSeed=seed;
  fSRndm.SetSeed(seed);

  for(Int_t i=0; i<fSInstances.Count(); ++i) {
    ((QProcRandom3*)fSInstances[i])->SetInitSeed((UInt_t)(4294967294u*fSRndm.Rndm())+1);
  }
}
