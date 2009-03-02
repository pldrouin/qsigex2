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

QDisTF::QDisTF(const Char_t* filename, const Char_t* objectname)
{
  PRINTF6(this,"\tQDisTF::QDisTF(const Char_t* filename<",filename,">, const Char_t* objectname<",objectname,">)\n")

  TDirectory *curdir=gDirectory;
  TFile f(filename,"READ");
  fTF=dynamic_cast<TF1*>(f.Get(objectname));

  if(!fTF){
    cout << "QDisTF::QDisTF: Object '" << objectname << "' in file '" << filename << "' doesn't exist\n"; 
    throw 1;
  }
  fNDims=fTF->GetNdim();

  f.Close();
  curdir->cd();
}

Double_t QDisTF::Eval(const Double_t &x0) const
{
    return (Double_t)const_cast<TF1*>(fTF)->Eval(x0);
}

Double_t QDisTF::Eval(const Double_t &x0, const Double_t &x1) const
{
    return (Double_t)const_cast<TF1*>(fTF)->Eval(x0,x1);
}

Double_t QDisTF::Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2) const
{
    return (Double_t)const_cast<TF1*>(fTF)->Eval(x0,x1,x2);
}

Double_t QDisTF::Eval(Double_t const* const &x) const
{
  switch(fTF->GetNdim()) {
    case 1:
      return (Double_t)const_cast<TF1*>(fTF)->Eval(x[0]);
    case 2:
      return (Double_t)const_cast<TF1*>(fTF)->Eval(x[0],x[1]);
    case 3:
      return (Double_t)const_cast<TF1*>(fTF)->Eval(x[0],x[1],x[2]);
  }
  return 0;
}

Double_t QDisTF::Integral(Double_t xlo, Double_t xhi, Double_t ylo, Double_t yhi, Double_t zlo, Double_t zhi) const
{
  switch(fTF->GetNdim()){
    case 1:
      return fTF->Integral(xlo,xhi);
      break;
    case 2:
      return fTF->Integral(xlo,xhi,ylo, yhi);
      break;
    case 3:
      return fTF->Integral(xlo,xhi,ylo, yhi, zlo, zhi);
      break;
    default:
      return 0;
  }
}

Double_t QDisTF::Integral(Option_t* domain) const
{
  PRINTF4(this,"\tDouble_t QDisTF::Integral(Option_t* domain<",domain,">) const\n")

  Double_t xmin,xmax,ymin,ymax,zmin,zmax;
  fTF->GetRange(xmin,ymin,zmin,xmax,ymax,zmax);

  if(!domain || !strlen(domain)) return Integral(xmin,xmax,ymin,ymax,zmin,zmax);

  TString objformula=fTF->GetExpFormula();
  TString newformula="(";
  newformula+=objformula;
  newformula+=")*(";
  newformula+=domain;
  newformula+=")";
  
  TF1 *tfcut=dynamic_cast<TF1*>(fTF->Clone());
  tfcut->SetTitle(newformula);
  tfcut->Compile();

  for(Int_t i=0;i<fTF->GetNpar();i++){
    tfcut->SetParameter(i,fTF->GetParameter(i));
  }

  TF1* oldtf=fTF;
  fTF=tfcut;
  Double_t integral=Integral(xmin,xmax,ymin,ymax,zmin,zmax);
  fTF=oldtf;
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

    TString objformula=fTF->GetExpFormula();

    Double_t cutintbuf=Integral(fCutExpr);

    if(integral) *integral=cutintbuf;

    if (cutintbuf) {
      TString newformula;
      newformula="(";
      newformula+=objformula;
      newformula+=")/";
      newformula+=cutintbuf;
      fTF->SetTitle(newformula);
      fTF->Compile();
    }
  } catch (Int_t e){
    cout << "Exception handled by QDisTF::Normalize\n";
    throw e;
  }
}

#include "debugger.h"

