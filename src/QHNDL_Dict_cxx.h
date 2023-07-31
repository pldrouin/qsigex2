// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

template <> inline void QHNDL<QPHData>::Streamer(TBuffer &){}
template <> inline void QHNDL<QPHEData>::Streamer(TBuffer &){}

template <typename U> void QHNDL<U>::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QHNDL<U>.

  UInt_t R__s, R__c;

  if (R__b.IsReading()) {
    R__b.ReadVersion(&R__s, &R__c);
    QHNF<U>::Streamer(R__b);
    R__b.CheckByteCount(R__s, R__c, QHNDL<U>::IsA());

  } else {
    R__c = R__b.WriteVersion(QHNDL<U>::IsA(), kTRUE);
    QHNF<U>::Streamer(R__b);
    R__b.SetByteCount(R__c, kTRUE);
  }
}
