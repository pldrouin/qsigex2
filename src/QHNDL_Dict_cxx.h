// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

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
