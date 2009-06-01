template <typename U> void QHNF<U>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QHN<U>.

  UInt_t R__s, R__c;
  Int_t i;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c);
    QDis::Streamer(R__b);
    R__b >> QHN<U>::fNDims;

    if(QHN<U>::fAxes) delete[] QHN<U>::fAxes;
    QHN<U>::fAxes=new QAxis*[QHN<U>::fNDims];

    for(i=0; i<QHN<U>::fNDims; i++) {
      R__b >> QHN<U>::fAxes[i]; 
    }
    R__b >> QHN<U>::fEntries;
    R__b >> fNFBins;

    if(fBins) {
      free(fBins);
      free(QHN<U>::fBinContent);
    }
    fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
    QHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));

    R__b.ReadFastArray(fBins,fNFBins);
    R__b.ReadFastArray(QHN<U>::fBinContent,fNFBins);
    if(R__v>1) R__b >> QHN<U>::fIntUseFBinLoop;
    ComputeNBins();

    R__b.CheckByteCount(R__s, R__c, QHN<U>::IsA());
  } else {
    QAxis *abuf;
    R__c = R__b.WriteVersion(QHN<U>::IsA(), kTRUE);
    QDis::Streamer(R__b);
    R__b << QHN<U>::fNDims;

    for(i=0; i<QHN<U>::fNDims; i++) {

      if(!QHN<U>::fAxes[i]) {
	abuf=new QAxis;
	R__b << abuf;
	delete abuf;

      } else {
	R__b << QHN<U>::fAxes[i];
      }
    }
    R__b << QHN<U>::fEntries;
    R__b << fNFBins;

    R__b.WriteFastArray(fBins,fNFBins);
    R__b.WriteFastArray(QHN<U>::fBinContent,fNFBins);
    R__b << QHN<U>::fIntUseFBinLoop;

    R__b.SetByteCount(R__c, kTRUE);
  }
}
