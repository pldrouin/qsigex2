#include "QPHN.h"

ClassImp(QPHN)

void QPHN::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QPHN>.

  UInt_t R__s, R__c;
  Int_t i;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
    QDis::Streamer(R__b);
    R__b >> fNDims;

    if(fAxes) delete[] fAxes;
    fAxes=new QAxis*[fNDims];

    for(i=0; i<fNDims; i++) {
      R__b >> fAxes[i]; 
    }
    ComputeNBins();
    R__b >> fNBins;
    fBinContent=(QPHData*)malloc(fNBins*sizeof(QPHData));

    for(Int_t i=0; i<fNBins; i++) {
      R__b >> fBinContent[i].fContent;
      R__b >> fBinContent[i].fEntries;
    }
    if(R__v>1) R__b >> fIntUseFBinLoop;

    R__b.CheckByteCount(R__s, R__c, QPHN::IsA());

  } else {
    QAxis *abuf;
    R__c = R__b.WriteVersion(QPHN::IsA(), kTRUE);
    QDis::Streamer(R__b);
    R__b << fNDims;

    for(i=0; i<fNDims; i++) {

      if(!fAxes[i]) {
	abuf=new QAxis;
	R__b << abuf;
	delete abuf;

      } else {
	R__b << fAxes[i];
      }
    }
    R__b << fEntries;
    R__b << fNBins;

    for(Int_t i=0; i<fNBins; i++) {
      R__b << fBinContent[i].Content();
      R__b << fBinContent[i].Entries();
    }
    R__b << fIntUseFBinLoop;

    R__b.SetByteCount(R__c, kTRUE);
  }
}
