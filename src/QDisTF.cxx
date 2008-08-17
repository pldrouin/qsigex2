// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QDisTF.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTF                                                           //
//                                                                  //
// This class creates a probability density function from a TF      //
// object. The class is derived from abstract base class QDis       //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

ClassImp(QDisTF)

QTFOps QDisTF::fQTFOps; //fQTFOps will perform operations on the pdf

Double_t QDisTF::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const
{
  //This function returns the probability density associated with a point which
  //coordinates are (x,y,z). For p.d.f. with less than 3 dimensions, the
  //arguments of extra dimensions are optional. Before calling this function,
  //the user must call QDisTF::Normalize() to normalize the p.d.f. properly. 

  PRINTF2(this,"\tDouble_t QDisTF::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const\n")

  try{
    fQTFOps.SetTF(dynamic_cast<TF1*>(GetObject())); //Set the pdf.  GetObject() is a QTObjectIO method.

    return fQTFOps.Freq(x,y,z);

  } catch (Int_t i){
    cout << "Exception handled by QDisTF::ProbDensity\n";
    throw i;
  }
}


Double_t QDisTF::Derivative(const Double_t &x) const
{
  // This function returns the derivative of the pdf at x
  // this function only works for one dimensional pdfs

   try{
    fQTFOps.SetTF(dynamic_cast<TF1*>(GetObject())); //Set the pdf.  GetObject() is a QTObjectIO method.

    return fQTFOps.Derivative(x);

  } catch (Int_t i){
    cout << "Exception handled by QDisTF::Derivative\n";
    throw i;
  } 
}

void QDisTF::Normalize(Double_t* integral)
{
  //This function normalizes the p.d.f. according to the cuts defined via
  //SetCutExpr. This string is a standard ROOT selection expression that
  //contains x and/or y and/or z variables. The argument
  //(optional) is filled with the function integral.

  PRINTF4(this,"\tvoid QDSigExisTF::Normalize(Double_t* integral<",integral,">)\n")

  try{
    if(fNormFlags){
      cout << "Error: QDisTF::Normalize: Unknown flag(s)\n";
      throw 1;
    }

    TF1* obj=dynamic_cast<TF1*>(GetObject());

    TString objformula=obj->GetExpFormula();

    fQTFOps.SetTF(dynamic_cast<TF1*>(GetObject()));  //set the pdf.  GetObject is a QTObjectIO method.

    Double_t cutintbuf=fQTFOps.LimIntegral(fCutExpr);

    if(integral) *integral=cutintbuf;

    if (cutintbuf) {
      TString newformula;
      newformula="(";
      newformula+=objformula;
      newformula+=")/";
      newformula+=cutintbuf;
      obj->SetTitle(newformula);
      obj->Compile();
    }
  } catch (Int_t e){
    cout << "Exception handled by QDisTF::Normalize\n";
    throw e;
  }
}

#include "debugger.h"

