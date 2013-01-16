// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QProcRandom3.h"

ClassImp(QProcRandom3)

UInt_t QProcRandom3::fInitGSeed(4357);
TRandom3 QProcRandom3::fSRndm(4357);
QList<QProcObj*> QProcRandom3::fSInstances;

void QProcRandom3::SetGSeed(const UInt_t &seed)
{
  fInitGSeed=seed;
  fSRndm.SetSeed(seed);

  for(Int_t i=0; i<fSInstances.Count(); ++i) {
    ((QProcRandom3*)fSInstances[i])->SetInitSeed((UInt_t)(4294967294u*fSRndm.Rndm())+1);
  }
}
