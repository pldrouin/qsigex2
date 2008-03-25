// Author: Pierre-Luc Drouin <http://www.pldrouin.net>, Osama Moussa <http://www.physics.carleton.ca/~omoussa>
// Copyright Carleton University

#ifndef _QSIGEXTHOPS_   //not sure why the leading underscore is ok here...   
#define _QSIGEXTHOPS_

#include "Rtypes.h"
#include "TH1.h"  //Root class TH1
#include "TFormula.h"
#include <iostream>

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExTHOps                                                          //
//                                                                      //
// Class performing calculations on histograms                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QSigExTHOps
{
 public:
  //constructor, initializes member variable fTH
  QSigExTHOps(const TH1* fTHIn):fTH(const_cast<TH1*>(fTHIn)){PRINTF4(this,"\tQSigExTHOps::QSigExTHOps(const TH1* fTHIn<",fTHIn,">)\n") } 

  //constructor with no arguments initializes fTH to NULL
  QSigExTHOps():fTH(NULL){PRINTF2(this,"\tQSigExTHOps::QSigExTHOps()\n") }

  QSigExTHOps(const QSigExTHOps& rhs):fTH(rhs.fTH){}

  //destructor
  virtual ~QSigExTHOps(){PRINTF2(this,"\tQSigExTHOps::~QSigExTHOps()\n") }

  //Functions to return values in bins, and integrals...
  Double_t Freq(Double_t x, Double_t y=0, Double_t z=0) const; 

  Double_t BinIntegral(Int_t xbinlo=1,Int_t xbinhi=-1,Int_t ybinlo=1,Int_t ybinhi=-1, Int_t zbinlo=1, Int_t zbinhi=-1) const; 

  Double_t LimIntegral(Option_t* domain=NULL, Double_t* error=NULL, Int_t** binranges=NULL) const;

  Double_t LimIntegral(Double_t xlo,Double_t xhi,Double_t ylo=0,Double_t yhi=0, Double_t zlo=0, Double_t zhi=0) const; 

  //function to return a pointer to the histogram fTH.
  const TH1* GetTH() const{PRINTF2(this,"\tconst TH1* QSigExTHOps::GetTH()\n") return fTH;}


  void SetTH(const TH1* th){PRINTF4(this,"\tvoid QSigExTHOps::SetTH2(const TH2* th2<",th2,">)\n") fTH=th; if(!fTH){ cout << "QSigExTHOps::SetTH: Passed pointer NULL\n"; throw 1;}}


 protected:
  void CheckTH() const;
 private:
  const TH1* fTH; //!
  ClassDef(QSigExTHOps,1) //Performs calculations on histograms
};

#include "debugger.h"

#endif






