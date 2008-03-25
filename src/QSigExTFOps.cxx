// Author: Pierre-Luc Drouin <http://www.pldrouin.net>, Osama Moussa <http://www.physics.carleton.ca/~omoussa>
// Copyright Carleton University

#include "QSigExTFOps.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExTFOps                                                          //
//                                                                      //
// Class performing calculations on histograms                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExTFOps)

Double_t QSigExTFOps::Freq(Double_t x, Double_t y, Double_t z) const
{
  PRINTF8(this,"\tInt_t QSigExTFOps::Freq(Double_t x<",x,">, Double_t y<",y,">, Double_t z<",z,">)\n")

  CheckTF(); //make sure there is actually an active histogram

  return (Double_t)const_cast<TF1*>(fTF)->Eval(x,y,z);
}

Double_t QSigExTFOps::Derivative(Double_t x) const
{
  PRINTF4(this,"\tInt_t QSigExTFOps::Derivative(Double_t x<",x,">)\n")

  CheckTF(); //make sure there is actually an active histogram
  if(fTF->GetNdim()>1){
    cout <<"ERROR <QSigExTFOps::Derivative> only available for 1D TF objects.\n";
    throw 222;
  }

  return (Double_t)const_cast<TF1*>(fTF)->Derivative(x);
}


Double_t QSigExTFOps::LimIntegral(Double_t xlo, Double_t xhi, Double_t ylo, Double_t yhi, Double_t zlo, Double_t zhi) const
{

  CheckTF(); //check that there is an active histogram

  TF1* ctf=const_cast<TF1*>(fTF);

  switch(fTF->GetNdim()){
    case 1:
      return ctf->Integral(xlo,xhi);
      break;
    case 2:
      return ctf->Integral(xlo,xhi,ylo, yhi);
      break;
    case 3:
      return ctf->Integral(xlo,xhi,ylo, yhi, zlo, zhi);
      break;
    default:
      return 0;
  }
}

Double_t QSigExTFOps::LimIntegral(Option_t* domain, Double_t* error)
{
  PRINTF6(this,"\tDouble_t QSigExTFOps::LimIntegral(Option_t* domain<",domain,">, Double_t* error<",error,">) const\n")

  error=NULL;

  CheckTF();

  Double_t xmin,xmax,ymin,ymax,zmin,zmax;
  const TF1* obj=dynamic_cast<const TF1*>(fTF);
  obj->GetRange(xmin,ymin,zmin,xmax,ymax,zmax);

  if(!domain) return LimIntegral(xmin,xmax,ymin,ymax,zmin,zmax);

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

  const TF1* oldtf=fTF;
  fTF=tfcut;
  Double_t integral=LimIntegral(xmin,xmax,ymin,ymax,zmin,zmax);
  fTF=oldtf;
  delete tfcut;
  return integral;

}

void QSigExTFOps::CheckTF() const
{
  // Throws an exception if no instance is associated with fTF.

  if(!fTF){
    cout << "Error: QSigExTFOps::CheckTF(): fTF==NULL. fTF must hold the address of a TF1*. Use a try-catch exception handler block if you want that your program to handle this exception and not abort.\n";
    throw 1;
  }
}

#include "debugger.h"
