// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef __INTEL_COMPILER
#ifndef GCC_VERSION
#define GCC_VERSION (__GNUC__ * 100 + __GNUC_MINOR__)
#endif

#if GCC_VERSION >= 403
# define EXTERN
#else
# define EXTERN extern
#endif
#else
# define EXTERN
#endif

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
template <> EXTERN void QList<UChar_t>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<Bool_t>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<Int_t>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<Float_t>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<Double_t>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<void*>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<Double_t*>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<TObject*>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<QProcObj const*>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<QProcObj*>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<QProcArray*>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<QOversizeArray*>::Streamer(TBuffer &R__b);
template <> EXTERN void QList<QDepTree*>::Streamer(TBuffer &R__b);
#else
template <> void QList<UChar_t>::Streamer(TBuffer &R__b)
{
  // Stream an object of class QList<UChar_t>.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Int_t nelements;
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    TObject::Streamer(R__b);
    R__b >> nelements;
    RedimList(nelements);
    R__b.ReadFastArray(fUArray,fNElements);
    R__b.CheckByteCount(R__s, R__c, QList<UChar_t>::IsA());
  } else {
    R__c = R__b.WriteVersion(QList<UChar_t>::IsA(), kTRUE);
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

template <> void QList<void*>::Streamer(TBuffer &){}
template <> void QList<Double_t*>::Streamer(TBuffer &){}
template <> void QList<TObject*>::Streamer(TBuffer &){}
template <> void QList<QProcObj const*>::Streamer(TBuffer &){}
template <> void QList<QProcArray*>::Streamer(TBuffer &){}
template <> void QList<QOversizeArray*>::Streamer(TBuffer &){}
template <> void QList<QDepTree*>::Streamer(TBuffer &){}

#endif
