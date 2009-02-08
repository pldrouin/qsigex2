template <typename U> void QTHN<U>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QTHN<U>.

  UInt_t R__s, R__c;
  Int_t i;
  Long64_t li;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TNamed::Streamer(R__b);
    R__b >> fNDims;
    R__b >> fEntries;

    if(fAxes) delete[] fAxes;
    fAxes=new TAxis*[fNDims];

    for(i=0; i<fNDims; i++) {
      R__b >> fAxes[i]; 
    }

    R__b >> fNFBins;
    if(fBins) {
      free(fBins);
      free(fFBinContent);
    }
    fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
    fFBinContent=(U*)malloc(fNFBins*sizeof(U));

    for(li=0; li<fNFBins; li++) {
      R__b >> fBins[li];
      R__b >> fFBinContent[li];
    }
    ComputeNBins();
    R__b.CheckByteCount(R__s, R__c, QTHN<U>::IsA());
  } else {
    TAxis *abuf;
    R__c = R__b.WriteVersion(QTHN<U>::IsA(), kTRUE);
    TNamed::Streamer(R__b);
    R__b << fNDims;
    R__b << fEntries;

    for(i=0; i<fNDims; i++) {

      if(!fAxes[i]) {
	abuf=new TAxis;
	R__b << abuf;
	delete abuf;

      } else {
	R__b << fAxes[i];
      }
    }

    R__b << fNFBins;

    for(li=0; li<fNFBins; li++) {
      R__b << fBins[li];
      R__b << fFBinContent[li];
    }
    R__b.SetByteCount(R__c, kTRUE);
  }
}
