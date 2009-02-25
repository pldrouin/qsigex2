template <typename U> void QTHNF<U>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QTHN<U>.

  UInt_t R__s, R__c;
  Int_t i;
  Long64_t li;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TNamed::Streamer(R__b);
    R__b >> QTHN<U>::fNDims;

    if(QTHN<U>::fAxes) delete[] QTHN<U>::fAxes;
    QTHN<U>::fAxes=new TAxis*[QTHN<U>::fNDims];

    for(i=0; i<QTHN<U>::fNDims; i++) {
      R__b >> QTHN<U>::fAxes[i]; 
    }
    ComputeNBins();
    R__b >> QTHN<U>::fEntries;
    R__b >> fNFBins;

    if(fBins) {
      free(fBins);
      free(QTHN<U>::fBinContent);
    }
    fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
    QTHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));

    for(li=0; li<fNFBins; li++) {
      R__b >> fBins[li];
      R__b >> QTHN<U>::fBinContent[li];
    }
    R__b.CheckByteCount(R__s, R__c, QTHN<U>::IsA());
  } else {
    TAxis *abuf;
    R__c = R__b.WriteVersion(QTHN<U>::IsA(), kTRUE);
    TNamed::Streamer(R__b);
    R__b << QTHN<U>::fNDims;

    for(i=0; i<QTHN<U>::fNDims; i++) {

      if(!QTHN<U>::fAxes[i]) {
	abuf=new TAxis;
	R__b << abuf;
	delete abuf;

      } else {
	R__b << QTHN<U>::fAxes[i];
      }
    }
    R__b << QTHN<U>::fEntries;
    R__b << fNFBins;

    for(li=0; li<fNFBins; li++) {
      R__b << fBins[li];
      R__b << QTHN<U>::fBinContent[li];
    }
    R__b.SetByteCount(R__c, kTRUE);
  }
}
