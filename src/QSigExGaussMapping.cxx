#include "QSigExGaussMapping.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

Double_t QSigExGaussMapping(Double_t * x, Double_t* params){
  QSigExStaticList list;

  if(list.GetSize()){
  const TH1F* thbuf=dynamic_cast<const TH1F*>(list.At(0));
  const TAxis* xaxis=thbuf->GetXaxis();
  Int_t bin=xaxis->FindFixBin(*x);
  Int_t err;
  //  static Double_t lastx;
  params=NULL;

  Double_t cumul=thbuf->Integral(1,bin,"width")-(xaxis->GetBinUpEdge(bin)-*x)*(thbuf->GetBinContent(bin));
  Double_t ret= TMath::Sqrt(2.)*erfinv(2*cumul-1,err);
  //if(cumul>=1){
  //  printf("%lf\t%lf\t%i\t%lf\t%lf\n",*x,xaxis->GetBinLowEdge(bin),bin,thbuf->GetBinContent(bin),cumul);
  //}

  //  if(err){
  //    if((*params) && !(*(params+1))) *(params+1)=lastx;
  //  }else if(!(*params)) *params=*x;
  
  //  lastx=*x;

  return ret;
  }else{
    cout << "Error in QSigExGaussMapping: the QSigExStaticList object is empty\n";
    throw 1;
  }
}

#include "debugger.h"
