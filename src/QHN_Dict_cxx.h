template <typename U> void QHN<U>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QHN<U>.

  UInt_t R__s, R__c;
  Int_t i;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
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

    R__b.SetByteCount(R__c, kTRUE);
  }
}
