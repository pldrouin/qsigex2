// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

template <typename U> void QHN<U>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QHN<U>.

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
    R__b >> fEntries;
    R__b.ReadFastArray(fBinContent,fNBins);
    if(R__v>1) R__b >> fIntUseFBinLoop;

    R__b.CheckByteCount(R__s, R__c, QHN<U>::IsA());

  } else {
    QAxis *abuf;
    R__c = R__b.WriteVersion(QHN<U>::IsA(), kTRUE);
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
    R__b.WriteFastArray(fBinContent,fNBins);
    R__b << fIntUseFBinLoop;

    R__b.SetByteCount(R__c, kTRUE);
  }
}
