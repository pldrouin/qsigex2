// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

template <> inline void QHNF<QPHData>::Streamer(TBuffer &){}
template <> inline void QHNF<QPHEData>::Streamer(TBuffer &){}

#ifndef _QHNF_SPECIALS_
template <> EXTERN void QHNF<QHEData<Double_t> >::Streamer(TBuffer &R__b);
template <> EXTERN void QHNF<QHEData<Float_t> >::Streamer(TBuffer &R__b);
#else
#define QHNF_QHE_Streamer(T) \
template <> void QHNF<QHEData<T> >::Streamer(TBuffer &R__b)\
{\
  UInt_t R__s, R__c;\
  Int_t i;\
\
  if (R__b.IsReading()) {\
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c);\
    QDis::Streamer(R__b);\
    R__b >> QHN<QHEData<T> >::fNDims;\
\
    if(QHN<QHEData<T> >::fAxes) delete[] QHN<QHEData<T> >::fAxes;\
    QHN<QHEData<T> >::fAxes=new QAxis*[QHN<QHEData<T> >::fNDims];\
\
    for(i=0; i<QHN<QHEData<T> >::fNDims; i++) {\
      R__b >> QHN<QHEData<T> >::fAxes[i]; \
    }\
    R__b >> QHN<QHEData<T> >::fEntries;\
    R__b >> fNFBins;\
\
    if(fBins) {\
      free(fBins);\
      free(QHN<QHEData<T> >::fBinContent);\
    }\
    fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));\
    QHN<QHEData<T> >::fBinContent=(QHEData<T> *)malloc(fNFBins*sizeof(QHEData<T> ));\
\
    R__b.ReadFastArray(fBins,fNFBins);\
    R__b.ReadFastArray(QHN<QHEData<T> >::fBinContent, TClass::GetClass<QHEData<T> >(), fNFBins);\
    if(R__v>1) R__b >> QHN<QHEData<T> >::fIntUseFBinLoop;\
    ComputeNBins();\
\
    R__b.CheckByteCount(R__s, R__c, QHN<QHEData<T> >::IsA());\
  } else {\
    QAxis *abuf;\
    R__c = R__b.WriteVersion(QHN<QHEData<T> >::IsA(), kTRUE);\
    QDis::Streamer(R__b);\
    R__b << QHN<QHEData<T> >::fNDims;\
\
    for(i=0; i<QHN<QHEData<T> >::fNDims; i++) {\
\
      if(!QHN<QHEData<T> >::fAxes[i]) {\
	abuf=new QAxis;\
	R__b << abuf;\
	delete abuf;\
\
      } else {\
	R__b << QHN<QHEData<T> >::fAxes[i];\
      }\
    }\
    R__b << QHN<QHEData<T> >::fEntries;\
    R__b << fNFBins;\
\
    R__b.WriteFastArray(fBins,fNFBins);\
    R__b.WriteFastArray(QHN<QHEData<T> >::fBinContent, TClass::GetClass<QHEData<T> >(), fNFBins);\
    R__b << QHN<QHEData<T> >::fIntUseFBinLoop;\
\
    R__b.SetByteCount(R__c, kTRUE);\
  }\
}
QHNF_QHE_Streamer(Double_t)
QHNF_QHE_Streamer(Float_t)
#endif

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
