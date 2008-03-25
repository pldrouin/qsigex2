// Author: Pierre-Luc Drouin <http://www.pldrouin.net>, Osama Moussa <http://www.physics.carleton.ca/~omoussa>
// Copyright Carleton University

#ifndef _QSIGEXTFOPS_   //not sure why the leading underscore is ok here...   
#define _QSIGEXTFOPS_

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
// QSigExTFOps                                                          //
//                                                                      //
// Class performing calculations on histograms                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QSigExTFOps
{
 public:
  //constructor, initializes member variable fTF
  QSigExTFOps(const TF1* fTFIn):fTF(const_cast<TF1*>(fTFIn)){PRINTF4(this,"\tQSigExTFOps::QSigExTFOps(const TF1* fTFIn<",fTFIn,">)\n") } 

  //constructor with no arguments initializes fTF to NULL
  QSigExTFOps():fTF(NULL){PRINTF2(this,"\tQSigExTFOps::QSigExTFOps()\n") }

  QSigExTFOps(const QSigExTFOps& rhs):fTF(rhs.fTF){}

  //destructor
  virtual ~QSigExTFOps(){PRINTF2(this,"\tQSigExTFOps::~QSigExTFOps()\n") }

  //Functions to return values in bins, and integrals...
  Double_t Freq(Double_t x, Double_t y=0, Double_t z=0) const; 
  Double_t Derivative(Double_t x) const; 

  Double_t LimIntegral(Option_t* domain=NULL, Double_t* error=NULL);

  Double_t LimIntegral(Double_t xlo,Double_t xhi,Double_t ylo=0,Double_t yhi=0, Double_t zlo=0, Double_t zhi=0) const; 

  //function to return a pointer to the histogram fTF.
  const TF1* GetTF() const{PRINTF2(this,"\tconst TF1* QSigExTFOps::GetTF()\n") return fTF;}


  void SetTF(const TF1* tf){PRINTF4(this,"\tvoid QSigExTFOps::SetTF2(const TF2* tf<",tf,">)\n") fTF=tf; if(!fTF){ cout << "QSigExTFOps::SetTF: Passed pointer NULL\n"; throw 1;}}


 protected:
  void CheckTF() const;
 private:
  const TF1* fTF; //!
  ClassDef(QSigExTFOps,1) //Performs calculations on TF* objects
};

#include "debugger.h"

#endif






