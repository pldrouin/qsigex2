// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExDisTF.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QSigExDisTF                                                      //
//                                                                  //
// This class creates a probability density function from a TF      //
// object. The class is derived from abstract base class QSigExDis  //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

ClassImp(QSigExDisTF)

QSigExTFOps QSigExDisTF::fQSigExTFOps; //fQSigExTFOps will perform operations on the pdf

Double_t QSigExDisTF::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const
{
  //This function returns the probability density associated with a point which
  //coordinates are (x,y,z). For p.d.f. with less than 3 dimensions, the
  //arguments of extra dimensions are optional. Before calling this function,
  //the user must call QSigExDisTF::Normalize() to normalize the p.d.f. properly. 

  PRINTF2(this,"\tDouble_t QSigExDisTF::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const\n")

  try{
    fQSigExTFOps.SetTF(dynamic_cast<TF1*>(GetObject())); //Set the pdf.  GetObject() is a QSigExIO method.

    return fQSigExTFOps.Freq(x,y,z);

  } catch (Int_t i){
    cout << "Exception handled by QSigExDisTF::ProbDensity\n";
    throw i;
  }
}


Double_t QSigExDisTF::Derivative(const Double_t &x) const
{
  // This function returns the derivative of the pdf at x
  // this function only works for one dimensional pdfs

   try{
    fQSigExTFOps.SetTF(dynamic_cast<TF1*>(GetObject())); //Set the pdf.  GetObject() is a QSigExIO method.

    return fQSigExTFOps.Derivative(x);

  } catch (Int_t i){
    cout << "Exception handled by QSigExDisTF::Derivative\n";
    throw i;
  } 
}

void QSigExDisTF::Normalize(Option_t* cutexpr, Int_t normflags, Double_t* fullintegral, Double_t* cutintegral, Double_t* error)
{
  //This function normalizes the p.d.f. according to the cuts defined via
  //cutexpr string. This string is a standard ROOT selection expression that
  //contains x and/or y and/or z variables. The second and third arguments
  //(optional) are filled with the total function integral and the integral of
  //function between cuts respectively. The fourth argument, when provided, is
  //set to 0. 

  PRINTF12(this,"\tvoid QDSigExisTF::Normalize(Option_t* cutexpr<",cutexpr,">, Int_t normflags<",normflags,">, Double_t* fullintegral<",fullintegral,">, Double_t* cutintegral<",cutintegral,">, Double_t* error<",error,">)\n")

  try{
    if(normflags){
      cout << "Error: QSigExDisTF::Normalize: Unknown flag(s)\n";
      throw 1;
    }

    TF1* obj=dynamic_cast<TF1*>(GetObject());

    TString objformula=obj->GetExpFormula();

    fQSigExTFOps.SetTF(dynamic_cast<TF1*>(GetObject()));  //set the pdf.  GetObject is a QSigExIO method.

    Double_t cutintbuf=fQSigExTFOps.LimIntegral(cutexpr,error);

    if(fullintegral){
      Double_t xmin,xmax,ymin,ymax,zmin,zmax;
      obj->GetRange(xmin,ymin,zmin,xmax,ymax,zmax);
      *fullintegral=fQSigExTFOps.LimIntegral(xmin,xmax,ymin,ymax,zmin,zmax);
    }
    if(cutintegral) *cutintegral=cutintbuf;

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
    cout << "Exception handled by QSigExDisTF::Normalize\n";
    throw e;
  }
}

#include "debugger.h"

