// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

//////////////////////////////////////////////////////////////////
//                                                              //
// QProgress                                                    //
//                                                              //
// This class prints on stderr computing progress information.  //
//                                                              //
//////////////////////////////////////////////////////////////////


#include "QProgress.h"

ClassImp(QProgress)

void QProgress::operator()(Int_t curval,Bool_t force)
{
  //This function refreshes the progress information on stderr only if it has
  //not been updated since a period of time equal or longer than the interval
  //specified by QProgress::SetInterval(). 

     Long_t time=GetTime();
  if((time-fLastTime)>=fInterval || force){
     fLastTime=time;
     Int_t runtime=(Int_t)((time-fInitTime)/1000.);
     Float_t frac=(Float_t)curval/fMaxVal;
     Float_t mfrac=(Float_t)(curval-fStartVal)/(fMaxVal-fStartVal);
     Int_t tottime=(Int_t)(runtime/mfrac);
     Int_t remtime=tottime-runtime;
    if(runtime>=1){
      fprintf(stderr,"\r[%2i:%02i]  %7i/%7i  (%3i%%)  [rem: %2i:%02i, total: %2i:%02i]",runtime/60,runtime%60,curval,fMaxVal,(Int_t)(100*frac),remtime/60,remtime%60,tottime/60,tottime%60);
    }else{
      fprintf(stderr,"\r[%2i:%02i]  %7i/%7i  (%3i%%)",runtime/60,runtime%60,curval,fMaxVal,(Int_t)(100*frac));
    }
  }
}

Long_t QProgress::GetTime(){
  //This private member function get the system time with a milisecond precision 

  return gSystem->Now();
}
