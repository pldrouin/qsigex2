template <typename U> void QTHNDL<U>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QTHNDL<U>.

  UInt_t R__s, R__c;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    QTHN<U>::Streamer(R__b);
    R__b.CheckByteCount(R__s, R__c, QTHNDL<U>::IsA());

  } else {
    R__c = R__b.WriteVersion(QTHNDL<U>::IsA(), kTRUE);
    QTHN<U>::Streamer(R__b);
    R__b.SetByteCount(R__c, kTRUE);
  }
}
