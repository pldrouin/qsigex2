#include "QPHN.h"

ClassImp(QPHN)

void QPHN::CopyStruct(const QHN_D &qthn)
{
  QHN_D::CopyStruct(qthn);
  const QPHN *qphn=dynamic_cast<const QPHN*>(&qthn);
  fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t));

  if(qphn) memcpy(fBinEntries,qphn->fBinEntries,fNBins*sizeof(Double_t));
  else for(Long64_t li=fNBins-1; li>=0; --li) fBinEntries[li]=1;
}

const QPHN& QPHN::operator=(const QPHN &qthn)
{
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
