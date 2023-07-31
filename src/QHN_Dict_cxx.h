// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

template <> inline void QHN<QPHData>::Streamer(TBuffer &){}
template <> inline void QHN<QPHEData>::Streamer(TBuffer &){}

#ifndef _QHN_SPECIALS_
template <> EXTERN void QHN<QHEData<Double_t> >::Streamer(TBuffer &R__b);
template <> EXTERN void QHN<QHEData<Float_t> >::Streamer(TBuffer &R__b);
#else
#define QHN_QHE_Streamer(T) \
template <> void QHN<QHEData<T> >::Streamer(TBuffer &R__b)\
{\
\
  UInt_t R__s, R__c;\
  Int_t i;\
\
  if (R__b.IsReading()) {\
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c);\
    QDis::Streamer(R__b);\
    R__b >> fNDims;\
\
    if(fAxes) delete[] fAxes;\
    fAxes=new QAxis*[fNDims];\
\
    for(i=0; i<fNDims; i++) {\
      R__b >> fAxes[i]; \
    }\
    ComputeNBins();\
    R__b >> fEntries;\
    R__b >> fNBins;\
    fBinContent=(QHEData<T>*)malloc(fNBins*sizeof(QHEData<T>));\
    R__b.ReadFastArray(fBinContent,TClass::GetClass<QHEData<T> >(), fNBins);\
    if(R__v>1) R__b >> fIntUseFBinLoop;\
    if(R__v>2) R__b >> fNormalizeAtTermination;\
\
    R__b.CheckByteCount(R__s, R__c, QHN<QHEData<T> >::IsA());\
\
  } else {\
    QAxis *abuf;\
    R__c = R__b.WriteVersion(QHN<QHEData<T> >::IsA(), kTRUE);\
    QDis::Streamer(R__b);\
    R__b << fNDims;\
\
    for(i=0; i<fNDims; i++) {\
\
      if(!fAxes[i]) {\
	abuf=new QAxis;\
	R__b << abuf;\
	delete abuf;\
\
      } else {\
	R__b << fAxes[i];\
      }\
    }\
    R__b << fEntries;\
    R__b << fNBins;\
    R__b.WriteFastArray(fBinContent, TClass::GetClass<QHEData<T> >(), fNBins);\
    R__b << fIntUseFBinLoop;\
    R__b << fNormalizeAtTermination;\
\
    R__b.SetByteCount(R__c, kTRUE);\
  }\
}
QHN_QHE_Streamer(Double_t)
QHN_QHE_Streamer(Float_t)
#endif

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
    if(R__v>2) R__b >> fNormalizeAtTermination;

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
    R__b << fNormalizeAtTermination;

    R__b.SetByteCount(R__c, kTRUE);
  }
}
