// Author: Pierre-Luc Drouin <http://www.pldrouin.net>, Osama Moussa <http://www.physics.carleton.ca/~omoussa>
// Copyright Carleton University

#ifndef _QTFOPS_   //not sure why the leading underscore is ok here...   
#define _QTFOPS_

#include "Rtypes.h"
#include "TF1.h"  //Root class TF1
#include "TFormula.h"
#include <iostream>

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QTFOps                                                               //
//                                                                      //
// Class performing calculations on histograms                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QTFOps
{
 public:
  //constructor, initializes member variable fTF
  QTFOps(const TF1* fTFIn):fTF(const_cast<TF1*>(fTFIn)){PRINTF4(this,"\tQTFOps::QTFOps(const TF1* fTFIn<",fTFIn,">)\n") } 

  //constructor with no arguments initializes fTF to NULL
  QTFOps():fTF(NULL){PRINTF2(this,"\tQTFOps::QTFOps()\n") }

  QTFOps(const QTFOps& rhs):fTF(rhs.fTF){}

  //destructor
  virtual ~QTFOps(){PRINTF2(this,"\tQTFOps::~QTFOps()\n") }

  //Functions to return values in bins, and integrals...
  Double_t Freq(Double_t x, Double_t y=0, Double_t z=0) const; 
  Double_t Derivative(Double_t x) const; 

  Double_t LimIntegral(Option_t* domain=NULL, Double_t* error=NULL);

  Double_t LimIntegral(Double_t xlo,Double_t xhi,Double_t ylo=0,Double_t yhi=0, Double_t zlo=0, Double_t zhi=0) const; 

  //function to return a pointer to the histogram fTF.
  const TF1* GetTF() const{PRINTF2(this,"\tconst TF1* QTFOps::GetTF()\n") return fTF;}


  void SetTF(const TF1* tf){PRINTF4(this,"\tvoid QTFOps::SetTF2(const TF2* tf<",tf,">)\n") fTF=tf; if(!fTF){ cout << "QTFOps::SetTF: Passed pointer NULL\n"; throw 1;}}


 protected:
  void CheckTF() const;
 private:
  const TF1* fTF; //!
  ClassDef(QTFOps,1) //Performs calculations on TF* objects
};

#include "debugger.h"

#endif






