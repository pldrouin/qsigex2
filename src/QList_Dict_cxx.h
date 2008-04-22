template <typename U> void QList<U>::Streamer(TBuffer &R__b)
{
  // Stream an object of class QList<U>.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Int_t nelements;
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TObject::Streamer(R__b);
    R__b >> nelements;
    RedimList(nelements);
    for(Int_t i=0; i<fNElements; i++) fUArray[i].Streamer(R__b);
    R__b.CheckByteCount(R__s, R__c, QList<U>::IsA());
  } else {
    R__c = R__b.WriteVersion(QList<U>::IsA(), kTRUE);
    TObject::Streamer(R__b);
    R__b << fNElements;
    for(Int_t i=0; i<fNElements; i++) fUArray[i].Streamer(R__b);
    R__b.SetByteCount(R__c, kTRUE);
  }
}

#ifndef _QLIST_SPECIALS_
template <> extern void QList<char>::Streamer(TBuffer &R__b);
template <> extern void QList<Bool_t>::Streamer(TBuffer &R__b);
template <> extern void QList<Int_t>::Streamer(TBuffer &R__b);
template <> extern void QList<Float_t>::Streamer(TBuffer &R__b);
template <> extern void QList<Double_t>::Streamer(TBuffer &R__b);
template <> extern void QList<Double_t*>::Streamer(TBuffer &R__b);
template <> extern void QList<TObject*>::Streamer(TBuffer &R__b);
#else
template <> void QList<char>::Streamer(TBuffer &R__b)
{
  // Stream an object of class QList<char>.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Int_t nelements;
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TObject::Streamer(R__b);
    R__b >> nelements;
    RedimList(nelements);
    R__b.ReadFastArray(fUArray,fNElements);
    R__b.CheckByteCount(R__s, R__c, QList<char>::IsA());
  } else {
    R__c = R__b.WriteVersion(QList<char>::IsA(), kTRUE);
    TObject::Streamer(R__b);
    R__b << fNElements;
    R__b.WriteFastArray(fUArray,fNElements);
    R__b.SetByteCount(R__c, kTRUE);
  }
}

template <> void QList<Bool_t>::Streamer(TBuffer &R__b)
{
  // Stream an object of class QList<Bool_t>.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Int_t nelements;
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TObject::Streamer(R__b);
    R__b >> nelements;
    RedimList(nelements);
    R__b.ReadFastArray(fUArray,fNElements);
    R__b.CheckByteCount(R__s, R__c, QList<Bool_t>::IsA());
  } else {
    R__c = R__b.WriteVersion(QList<Bool_t>::IsA(), kTRUE);
    TObject::Streamer(R__b);
    R__b << fNElements;
    R__b.WriteFastArray(fUArray,fNElements);
    R__b.SetByteCount(R__c, kTRUE);
  }
}

template <> void QList<Int_t>::Streamer(TBuffer &R__b)
{
  // Stream an object of class QList<Int_t>.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Int_t nelements;
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TObject::Streamer(R__b);
    R__b >> nelements;
    RedimList(nelements);
    R__b.ReadFastArray(fUArray,fNElements);
    R__b.CheckByteCount(R__s, R__c, QList<Int_t>::IsA());
  } else {
    R__c = R__b.WriteVersion(QList<Int_t>::IsA(), kTRUE);
    TObject::Streamer(R__b);
    R__b << fNElements;
    R__b.WriteFastArray(fUArray,fNElements);
    R__b.SetByteCount(R__c, kTRUE);
  }
}

template <> void QList<Float_t>::Streamer(TBuffer &R__b)
{
  // Stream an object of class QList<Float_t>.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Int_t nelements;
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TObject::Streamer(R__b);
    R__b >> nelements;
    RedimList(nelements);
    R__b.ReadFastArray(fUArray,fNElements);
    R__b.CheckByteCount(R__s, R__c, QList<Float_t>::IsA());
  } else {
    R__c = R__b.WriteVersion(QList<Float_t>::IsA(), kTRUE);
    TObject::Streamer(R__b);
    R__b << fNElements;
    R__b.WriteFastArray(fUArray,fNElements);
    R__b.SetByteCount(R__c, kTRUE);
  }
}

template <> void QList<Double_t>::Streamer(TBuffer &R__b)
{
  // Stream an object of class QList<Double_t>.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Int_t nelements;
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TObject::Streamer(R__b);
    R__b >> nelements;
    RedimList(nelements);
    R__b.ReadFastArray(fUArray,fNElements);
    R__b.CheckByteCount(R__s, R__c, QList<Double_t>::IsA());
  } else {
    R__c = R__b.WriteVersion(QList<Double_t>::IsA(), kTRUE);
    TObject::Streamer(R__b);
    R__b << fNElements;
    R__b.WriteFastArray(fUArray,fNElements);
    R__b.SetByteCount(R__c, kTRUE);
  }
}

template <> void QList<Double_t*>::Streamer(TBuffer &R__b)
{
  TBuffer *a;
  a=&R__b;
}

template <> void QList<TObject*>::Streamer(TBuffer &R__b)
{
  TBuffer *a;
  a=&R__b;
}
#endif
