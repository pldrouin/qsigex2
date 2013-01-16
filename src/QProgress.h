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

#ifndef _QPROGRESS_
#define _QPROGRESS_

#include <cstdio>
#include "Rtypes.h"
#include "TSystem.h"
#include "TTime.h"

class QProgress{
  public:
    QProgress(Int_t maxval=0, Int_t startval=0, Int_t interval=1000):fMaxVal(maxval),fStartVal(startval),fInterval(interval),fInitTime(GetTime()),fLastTime(GetTime()-interval){}
    virtual ~QProgress(){}
    void Init(Int_t startval=0){fStartVal=startval; fInitTime=GetTime(); fLastTime=fInitTime-fInterval;}
    void SetMaxVal(Int_t maxval){fMaxVal=maxval;}
    void SetInterval(Int_t interval){fInterval=interval;}
    Int_t GetMaxVal(){return fMaxVal;}
    void operator()(Int_t curval,Bool_t force=kFALSE);

  private:
    Long_t GetTime();
    Int_t fMaxVal;
    Int_t fStartVal;
    Int_t fInterval;
    Long_t fInitTime;
    Long_t fLastTime;

    ClassDef(QProgress,1) //Prints computing status on stderr
};


#endif
