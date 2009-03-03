#include "QPHN.h"

ClassImp(QPHN)

const QPHN& QPHN::operator=(const QPHN &qthn)
{
  Clear();
  QHN_D::operator=(qthn);
  fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t));
  memcpy(fBinEntries,qthn.fBinEntries,fNBins*sizeof(Double_t));
  return *this;
}

void QPHN::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QPHN.

  UInt_t R__s, R__c;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    QHN_D::Streamer(R__b);
    R__b.ReadFastArray(fBinEntries,fNBins);

    R__b.CheckByteCount(R__s, R__c, QPHN::IsA());

  } else {
    R__c = R__b.WriteVersion(QPHN::IsA(), kTRUE);
    QHN_D::Streamer(R__b);

    R__b.WriteFastArray(fBinEntries,fNBins);

    R__b.SetByteCount(R__c, kTRUE);
  }
}
