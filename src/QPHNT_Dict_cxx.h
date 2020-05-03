// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

template <typename DTYPE, typename QHBASE> void QPHNT<DTYPE, QHBASE>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QPHNT<DTYPE, QHASE >>.

  UInt_t R__s, R__c;
  Int_t i;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
    QDis::Streamer(R__b);
    R__b >> QHBASE::fNDims;

    if(QHBASE::fAxes) delete[] QHBASE::fAxes;
    QHBASE::fAxes=new QAxis*[QHBASE::fNDims];

    for(i=0; i<QHBASE::fNDims; i++) {
      R__b >> QHBASE::fAxes[i]; 
    }
    QHBASE::ComputeNBins();
    R__b >> QHBASE::fEntries;
    R__b >> QHBASE::fNBins;
    QHBASE::fBinContent=(DTYPE*)malloc(QHBASE::fNBins*sizeof(DTYPE));
    R__b.ReadFastArray(QHBASE::fBinContent, TClass::GetClass<DTYPE>(), QHBASE::fNBins);

    if(R__v>1) R__b >> QHBASE::fIntUseFBinLoop;

    R__b.CheckByteCount(R__s, R__c, QPHNT<DTYPE, QHBASE>::IsA());

  } else {
    QAxis *abuf;
    R__c = R__b.WriteVersion(QPHNT<DTYPE, QHBASE>::IsA(), kTRUE);
    QDis::Streamer(R__b);
    R__b << QHBASE::fNDims;

    for(i=0; i<QHBASE::fNDims; i++) {

      if(!QHBASE::fAxes[i]) {
	abuf=new QAxis;
	R__b << abuf;
	delete abuf;

      } else {
	R__b << QHBASE::fAxes[i];
      }
    }
    R__b << QHBASE::fEntries;
    R__b << QHBASE::fNBins;
    R__b.WriteFastArray(QHBASE::fBinContent, TClass::GetClass<DTYPE>(), QHBASE::fNBins);
    R__b << QHBASE::fIntUseFBinLoop;

    R__b.SetByteCount(R__c, kTRUE);
  }
}
