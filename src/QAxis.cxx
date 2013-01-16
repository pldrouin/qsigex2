// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QAxis.h"

ClassImp(QAxis)

void QAxis::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QAxis.

  UInt_t R__s, R__c;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    Clear();
    TNamed::Streamer(R__b);
    R__b >> fNBins;
    R__b >> fMin;
    R__b >> fMax;
    fBWidth=(fMax-fMin)/fNBins;
    Bool_t varbin;
    R__b >> varbin;

    if(varbin) {
      fBins=new Double_t[fNBins+1];
      R__b.ReadFastArray(fBins,fNBins+1);
    }

    R__b.CheckByteCount(R__s, R__c, QAxis::IsA());

  } else {
    R__c = R__b.WriteVersion(QAxis::IsA(), kTRUE);
    TNamed::Streamer(R__b);
    R__b << fNBins;
    R__b << fMin;
    R__b << fMax;

    if(fBins) {
      R__b << kTRUE;
      R__b.WriteFastArray(fBins,fNBins+1);

    } else {
      R__b << kFALSE;
    }

    R__b.SetByteCount(R__c, kTRUE);
  }
}
