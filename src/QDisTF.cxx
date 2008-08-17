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

Double_t QDisTF::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const
{
  //This function returns the probability density associated with a point which
  //coordinates are (x,y,z). For p.d.f. with less than 3 dimensions, the
  //arguments of extra dimensions are optional. Before calling this function,
  //the user must call QDisTF::Normalize() to normalize the p.d.f. properly. 

  PRINTF2(this,"\tDouble_t QDisTF::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const\n")

  try{

    return (Double_t)const_cast<TF1*>(dynamic_cast<TF1*>(GetObject()))->Eval(x,y,z);

  } catch (Int_t i){
    cout << "Exception handled by QDisTF::ProbDensity\n";
    throw i;
  }
}


Double_t QDisTF::Integral(Double_t xlo, Double_t xhi, Double_t ylo, Double_t yhi, Double_t zlo, Double_t zhi) const
{
  TF1* obj=dynamic_cast<TF1*>(GetObject());

  switch(obj->GetNdim()){
    case 1:
      return obj->Integral(xlo,xhi);
      break;
    case 2:
      return obj->Integral(xlo,xhi,ylo, yhi);
      break;
    case 3:
      return obj->Integral(xlo,xhi,ylo, yhi, zlo, zhi);
      break;
    default:
      return 0;
  }
}

Double_t QDisTF::Integral(Option_t* domain) const
{
  PRINTF4(this,"\tDouble_t QDisTF::Integral(Option_t* domain<",domain,">) const\n")

  Double_t xmin,xmax,ymin,ymax,zmin,zmax;
  const TF1* obj=dynamic_cast<TF1*>(GetObject());
  obj->GetRange(xmin,ymin,zmin,xmax,ymax,zmax);

  if(!domain || !strlen(domain)) return Integral(xmin,xmax,ymin,ymax,zmin,zmax);

  TString objformula=obj->GetExpFormula();
  TString newformula="(";
  newformula+=objformula;
  newformula+=")*(";
  newformula+=domain;
  newformula+=")";
  
  TF1 *tfcut=dynamic_cast<TF1*>(obj->Clone());
  tfcut->SetTitle(newformula);
  tfcut->Compile();

  for(Int_t i=0;i<obj->GetNpar();i++){
    tfcut->SetParameter(i,obj->GetParameter(i));
  }

  const TF1* oldtf=obj;
  obj=tfcut;
  Double_t integral=Integral(xmin,xmax,ymin,ymax,zmin,zmax);
  obj=oldtf;
  delete tfcut;
  return integral;
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

    Double_t cutintbuf=Integral(fCutExpr);

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

