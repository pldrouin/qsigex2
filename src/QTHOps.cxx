// Author: Pierre-Luc Drouin <http://www.pldrouin.net>, Osama Moussa <http://www.physics.carleton.ca/~omoussa>
// Copyright Carleton University

#include "QTHOps.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QTHOps                                                               //
//                                                                      //
// Class performing calculations on histograms                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QTHOps)

Double_t QTHOps::Freq(Double_t x, Double_t y, Double_t z) const
{
  //PRINTF4(this,"\tInt_t QTHOps::Freq(Double_t x<",x,">, Double_t y<",y,">, Double_t z<",z,">)\n")

  CheckTH(); //make sure there is actually an active histogram

  TH1* thbuf=const_cast<TH1*>(fTH);

  return (Double_t)fTH->GetBinContent((thbuf->GetXaxis())->FindFixBin(x),
      (thbuf->GetYaxis())->FindFixBin(y),(thbuf->GetZaxis())->FindFixBin(z));
}

Double_t QTHOps::BinIntegral(Int_t xbinlo, Int_t xbinhi, Int_t ybinlo, Int_t ybinhi, Int_t zbinlo, Int_t zbinhi) const
{

  CheckTH(); //check that there is an active histogram

  TH1* thbuf=const_cast<TH1*>(fTH);

  //if no limits are given, find the number of bins and integrate over all
  if(xbinhi==-1) xbinhi=(thbuf->GetXaxis())->GetNbins();
  if(ybinhi==-1) ybinhi=(thbuf->GetYaxis())->GetNbins();
  if(zbinhi==-1) zbinhi=(thbuf->GetYaxis())->GetNbins();

  //Integral takes the sum of all of the bin values btwn the limits
  //however, the option "width" then multiplies by the bin widths
  switch(fTH->GetDimension()){
    case 1:
      return fTH->Integral(xbinlo,xbinhi, "width");
      break;
    case 2:
      return fTH->Integral(xbinlo,xbinhi,ybinlo, ybinhi, "width");
      break;
    case 3:
      return fTH->Integral(xbinlo,xbinhi,ybinlo, ybinhi, zbinlo, zbinhi, "width");
      break;
    default:
      return 0;
  }
}

Double_t QTHOps::LimIntegral(Option_t* domain, Double_t* error, Int_t** binranges) const
{
  PRINTF8(this,"\tDouble_t QTH1Ops::LimIntegral(Option_t* domain<",domain,">, Double_t* error<",error,">, const Int_t** binranges<",binranges,">) const\n")

  if(!domain){
    if(error) *error=0;
    return BinIntegral();
  }

  CheckTH();

  Double_t integral=0;
  Double_t ebuf=0;
  Int_t dim=fTH->GetDimension();

  Double_t x[3];

  TFormula cuts("cuts",domain);

  Int_t i, j[3];

  Bool_t pos[3];
  memset(pos,0,dim*sizeof(Bool_t));
  Bool_t haschanged[3+1];
  memset(haschanged,0,(dim+1)*sizeof(Bool_t));
  Int_t level=0;

  Int_t nbins[3], mins[3], maxs[3];
  nbins[0]=fTH->GetNbinsX();
  nbins[1]=(dim>1 ? fTH->GetNbinsY() : 1);
  nbins[2]=(dim>2 ? fTH->GetNbinsZ() : 1);
  TAxis **axis=new TAxis*[3];
  axis[0]=fTH->GetXaxis();
  axis[1]=fTH->GetYaxis();
  axis[2]=fTH->GetZaxis();
  Bool_t lastpass=kFALSE,pass;

  for(i=0;i<3;i++){
    if(binranges && binranges[i]){
      mins[i]=(*(binranges[i])>1)?*(binranges[i]):1;
      maxs[i]=(*(binranges[i]+1)<nbins[i])?*(binranges[i]+1):nbins[i]; 
    } else {
      mins[i]=1;
      maxs[i]=nbins[i];
    }
    //cout << mins[i] << "\t" << maxs[i] << "\n";
  }

  for(j[0]=mins[0];j[0]<=maxs[0];j[0]++){
    for(j[1]=mins[1];j[1]<=maxs[1];j[1]++){
      for(j[2]=mins[2];j[2]<=maxs[2];j[2]++){
	//cout << "Bin " << j[0] << "\t" << j[1] << "\t" << j[2] << "\n";
	haschanged[level]=kFALSE;
	level=0;
	for(i=0;i<dim;i++){
	  x[i]=axis[i]->GetBinLowEdge(j[i])+(Int_t)(pos[level])*((axis[level])->GetBinWidth(j[level]))*(1-1E-6);
	}
	/*cout << "pos: ";
	for(i=dim-1;i>=0;i--){
	  cout << pos[i] << "\t";
	} 
	cout << "\n";*/
	pass=(Bool_t)cuts.EvalPar(x);
	while(level<dim){
	  /*cout << "Has changed: ";
	  for(i=dim-1;i>=0;i--){
	    cout << haschanged[i] << "\t";
	  } 
	  cout << "\n";*/

	  lastpass=pass;
	  haschanged[level]=kTRUE;
	  pos[level]=(!pos[level]);
	  x[level]=(axis[level])->GetBinLowEdge(j[level])+(Int_t)(pos[level])*((axis[level])->GetBinWidth(j[level]))*(1-1E-6);
	  level=0;
	  /*cout << "pos: ";
	  for(i=dim-1;i>=0;i--){
	    cout << pos[i] << "\t";
	  }
	  cout << "\n";*/
	  if((pass=(Bool_t)cuts.EvalPar(x)) ^ lastpass) break;
	  while(haschanged[level]){
	    haschanged[level]=kFALSE;
	    level++;
	    //cout << "Increase level to " << level << "\n";
	  }
	}

	if(lastpass & pass){
	  integral+=fTH->GetBinContent(j[0],j[1],j[2])*((binranges && binranges[0])?1:axis[0]->GetBinWidth(j[0]))*((binranges && binranges[1])?1:axis[1]->GetBinWidth(j[1]))*((binranges && binranges[2])?1:axis[2]->GetBinWidth(j[2]));
	}
	if(lastpass ^ pass && fTH->GetBinContent(j[0],j[1],j[2])){
	  //cout << "Had error\n";
	  ebuf+=fTH->GetBinContent(j[0],j[1],j[2])*((binranges && binranges[0])?1:axis[0]->GetBinWidth(j[0]))*((binranges && binranges[1])?1:axis[1]->GetBinWidth(j[1]))*((binranges && binranges[2])?1:axis[2]->GetBinWidth(j[2]));
	}
	/*if(!lastpass & !pass){
	  cout << "Bin " << j[0] << "\t" << j[1] << "\t" << j[2] << " excluded\n";
	}*/

      }
    }
  }

  delete[] axis;

  if(error) *error=ebuf;

  return integral;

}

Double_t QTHOps::LimIntegral(Double_t xlo, Double_t xhi, Double_t ylo, Double_t yhi, Double_t zlo, Double_t zhi) const
{
  //This function integrates fTH between the values valloX, valloY and 
  //valhiX, valhiY.
  CheckTH();
  TAxis* ax=const_cast<TH1*>(fTH)->GetXaxis();
  TAxis* ay=const_cast<TH1*>(fTH)->GetYaxis();
  TAxis* az=const_cast<TH1*>(fTH)->GetZaxis();

  Int_t xbinlo=ax->FindFixBin(xlo);
  Int_t ybinlo=ay->FindFixBin(ylo);
  Int_t zbinlo=az->FindFixBin(zlo);
  Int_t xbinhi=ax->FindFixBin(xhi);
  Int_t ybinhi=ay->FindFixBin(yhi);
  Int_t zbinhi=az->FindFixBin(zhi);

  return BinIntegral(xbinlo,xbinhi,ybinlo,ybinhi,zbinlo,zbinhi);
}

void QTHOps::CheckTH() const
{
  // Throws an exception if no instance is associated with fTH.

  if(!fTH){
    cout << "Error: QTHOps::CheckTH(): fTH==NULL. fTH must hold the address of a TH1*. Use a try-catch exception handler block if you want that your program to handle this exception and not abort.\n";
    throw 1;
  }
}

#include "debugger.h"
