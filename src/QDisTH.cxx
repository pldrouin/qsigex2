// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QDisTH.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTH                                                           //
//                                                                  //
// This class creates a probability density function from a TH      //
// object. The class is derived from abstract base class QDis       //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

ClassImp(QDisTH)

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, Float_t ylow, Float_t yhigh, Int_t nbinsz, Float_t zlow, Float_t zhigh):QDis()
{

  if(nbinsy==0) {
    SetObject("TH1F",new TH1F(name,title,nbinsx,xlow,xhigh));

  } else if(nbinsz==0) {
    SetObject("TH2F",new TH2F(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh));

  } else {
    SetObject("TH3F",new TH3F(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh));
  }
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, Float_t ylow, Float_t yhigh, Int_t nbinsz, const Float_t *zbins):QDis()
{
  if(nbinsz <=0) nbinsz=1;
  TH1* th=new TH3F(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins[0],zbins[nbinsz]);
  th->GetZaxis()->Set(nbinsz,zbins);
  SetObject("TH3F",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz, Float_t zlow, Float_t zhigh):QDis()
{
  if(nbinsy <=0) nbinsy=1;
  TH1* th=new TH3F(name,title,nbinsx,xlow,xhigh,nbinsy,ybins[0],ybins[nbinsy],nbinsz,zlow,zhigh);
  th->GetYaxis()->Set(nbinsy,ybins);
  SetObject("TH3F",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy, Float_t ylow, Float_t yhigh, Int_t nbinsz, Float_t zlow, Float_t zhigh):QDis()
{
  if(nbinsy==0) {
    SetObject("TH1F",new TH1F(name,title,nbinsx,xbins));

  } else if(nbinsz==0) {
    if(nbinsx <=0) nbinsx=1;
    TH1* th=new TH2F(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ylow,yhigh);
    th->GetXaxis()->Set(nbinsx,xbins);
    SetObject("TH2F",th);

  } else {
    if(nbinsx <=0) nbinsx=1;
    TH1* th=new TH3F(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
    th->GetXaxis()->Set(nbinsx,xbins);
    SetObject("TH3F",th);
  }
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz, const Float_t *zbins):QDis()
{
  if(nbinsy <=0) nbinsy=1;
  if(nbinsz <=0) nbinsz=1;
  TH1* th=new TH3F(name,title,nbinsx,xlow,xhigh,nbinsy,ybins[0],ybins[nbinsy],nbinsz,zbins[0],zbins[nbinsz]);
  th->GetYaxis()->Set(nbinsy,ybins);
  th->GetZaxis()->Set(nbinsz,zbins);
  SetObject("TH3F",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy, Float_t ylow, Float_t yhigh, Int_t nbinsz, const Float_t *zbins):QDis()
{
  if(nbinsx <=0) nbinsx=1;
  if(nbinsz <=0) nbinsz=1;
  TH1* th=new TH3F(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ylow,yhigh,nbinsz,zbins[0],zbins[nbinsz]);
  th->GetXaxis()->Set(nbinsx,xbins);
  th->GetZaxis()->Set(nbinsz,zbins);
  SetObject("TH3F",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz, Float_t zlow, Float_t zhigh):QDis()
{
  if(nbinsz==0) {
    if(nbinsx <=0) nbinsx=1;
    if(nbinsy <=0) nbinsy=1;
    TH1* th=new TH2F(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ybins[0],ybins[nbinsy]);
    th->GetXaxis()->Set(nbinsx,xbins);
    th->GetYaxis()->Set(nbinsy,ybins);
    SetObject("TH2F",th);
    SetNameTitleToObject();

  } else {
    if(nbinsx <=0) nbinsx=1;
    if(nbinsy <=0) nbinsy=1;
    TH1* th=new TH3F(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ybins[0],ybins[nbinsy],nbinsz,zlow,zhigh);
    th->GetXaxis()->Set(nbinsx,xbins);
    th->GetYaxis()->Set(nbinsy,ybins);
    SetObject("TH2F",th);
    SetNameTitleToObject();
  }
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz, const Float_t *zbins):QDis()
{

  if(nbinsy==0) {
    SetObject("TH1F",new TH1F(name,title,nbinsx,xbins));

  } else if(nbinsz==0) {
    SetObject("TH2F",new TH2F(name,title,nbinsx,xbins,nbinsy,ybins));

  } else {
    SetObject("TH3F",new TH3F(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins));
  }
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, Double_t ylow, Double_t yhigh, Int_t nbinsz, Double_t zlow, Double_t zhigh):QDis()
{

  if(nbinsy==0) {
    SetObject("TH1D",new TH1D(name,title,nbinsx,xlow,xhigh));

  } else if(nbinsz==0) {
    SetObject("TH2D",new TH2D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh));

  } else {
    SetObject("TH3D",new TH3D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh));
  }
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, Double_t ylow, Double_t yhigh, Int_t nbinsz, const Double_t *zbins):QDis()
{
  if(nbinsz <=0) nbinsz=1;
  TH1* th=new TH3D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins[0],zbins[nbinsz]);
  th->GetZaxis()->Set(nbinsz,zbins);
  SetObject("TH3D",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, const Double_t *ybins, Int_t nbinsz, Double_t zlow, Double_t zhigh):QDis()
{
  if(nbinsy <=0) nbinsy=1;
  TH1* th=new TH3D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins[0],ybins[nbinsy],nbinsz,zlow,zhigh);
  th->GetYaxis()->Set(nbinsy,ybins);
  SetObject("TH3D",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Double_t *xbins, Int_t nbinsy, Double_t ylow, Double_t yhigh, Int_t nbinsz, Double_t zlow, Double_t zhigh):QDis()
{
  if(nbinsy==0) {
    SetObject("TH1D",new TH1D(name,title,nbinsx,xbins));

  } else if(nbinsz==0) {
    if(nbinsx <=0) nbinsx=1;
    TH1* th=new TH2D(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ylow,yhigh);
    th->GetXaxis()->Set(nbinsx,xbins);
    SetObject("TH2D",th);

  } else {
    if(nbinsx <=0) nbinsx=1;
    TH1* th=new TH3D(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
    th->GetXaxis()->Set(nbinsx,xbins);
    SetObject("TH3D",th);
  }
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, const Double_t *ybins, Int_t nbinsz, const Double_t *zbins):QDis()
{
  if(nbinsy <=0) nbinsy=1;
  if(nbinsz <=0) nbinsz=1;
  TH1* th=new TH3D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins[0],ybins[nbinsy],nbinsz,zbins[0],zbins[nbinsz]);
  th->GetYaxis()->Set(nbinsy,ybins);
  th->GetZaxis()->Set(nbinsz,zbins);
  SetObject("TH3D",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Double_t *xbins, Int_t nbinsy, Double_t ylow, Double_t yhigh, Int_t nbinsz, const Double_t *zbins):QDis()
{
  if(nbinsx <=0) nbinsx=1;
  if(nbinsz <=0) nbinsz=1;
  TH1* th=new TH3D(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ylow,yhigh,nbinsz,zbins[0],zbins[nbinsz]);
  th->GetXaxis()->Set(nbinsx,xbins);
  th->GetZaxis()->Set(nbinsz,zbins);
  SetObject("TH3D",th);
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Double_t *xbins, Int_t nbinsy, const Double_t *ybins, Int_t nbinsz, Double_t zlow, Double_t zhigh):QDis()
{
  if(nbinsz==0) {
    if(nbinsx <=0) nbinsx=1;
    if(nbinsy <=0) nbinsy=1;
    TH1* th=new TH2D(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ybins[0],ybins[nbinsy]);
    th->GetXaxis()->Set(nbinsx,xbins);
    th->GetYaxis()->Set(nbinsy,ybins);
    SetObject("TH2D",th);
    SetNameTitleToObject();

  } else {
    if(nbinsx <=0) nbinsx=1;
    if(nbinsy <=0) nbinsy=1;
    TH1* th=new TH3D(name,title,nbinsx,xbins[0],xbins[nbinsx],nbinsy,ybins[0],ybins[nbinsy],nbinsz,zlow,zhigh);
    th->GetXaxis()->Set(nbinsx,xbins);
    th->GetYaxis()->Set(nbinsy,ybins);
    SetObject("TH2D",th);
    SetNameTitleToObject();
  }
  SetNameTitleToObject();
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Double_t *xbins, Int_t nbinsy, const Double_t *ybins, Int_t nbinsz, const Double_t *zbins):QDis()
{

  if(nbinsy==0) {
    SetObject("TH1D",new TH1D(name,title,nbinsx,xbins));

  } else if(nbinsz==0) {
    SetObject("TH2D",new TH2D(name,title,nbinsx,xbins,nbinsy,ybins));

  } else {
    SetObject("TH3D",new TH3D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins));
  }
  SetNameTitleToObject();
}

Int_t QDisTH::Fill(const Double_t &x)
{
  return dynamic_cast<TH1*>(GetObject())->Fill(x);
}

Int_t QDisTH::Fill(const Double_t &x, const Double_t &y)
{
  return dynamic_cast<TH2*>(GetObject())->Fill(x,y);
}

Int_t QDisTH::Fill(const Double_t &x, const Double_t &y, const Double_t &z)
{
  return dynamic_cast<TH3*>(GetObject())->Fill(x,y,z);
}

Double_t QDisTH::Integral(Int_t** binranges, Bool_t *widths) const
{
  PRINTF6(this,"\tDouble_t QDisTH::Integral(Int_t** binranges<",binranges,">, Bool_t* widths<",widths,">) const\n")

  TH1* histo=dynamic_cast<TH1*>(GetObject()); //Histogram pointer

  Int_t dim=histo->GetDimension();
  Int_t nbins[3], mins[3], maxs[3];
  nbins[0]=histo->GetNbinsX();
  nbins[1]=(dim>1 ? histo->GetNbinsY() : 1);
  nbins[2]=(dim>2 ? histo->GetNbinsZ() : 1);

  Int_t i, j[3];

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

  Double_t integral=0;
  Double_t binvol, binwidth2;

  TAxis **axes=new TAxis*[3];
  axes[0]=histo->GetXaxis();
  axes[1]=histo->GetYaxis();
  axes[2]=histo->GetZaxis();

  //If bin width is constant in all directions
  if(!axes[0]->GetXbins()->fN && !axes[1]->GetXbins()->fN && !axes[2]->GetXbins()->fN) {

    //If integrating using bin width for all axes
    if(!widths) {
      binvol=axes[0]->GetBinWidth(1);
      for(i=1; i<dim; i++) binvol*=axes[i]->GetBinWidth(1);

      //Else if using bin width for only some axes
    } else {
      binvol=1;
      for(i=0; i<dim; i++) if(widths[i]) binvol*=axes[i]->GetBinWidth(1);
    }

    for(j[2]=mins[2];j[2]<=maxs[2];j[2]++){
      for(j[1]=mins[1];j[1]<=maxs[1];j[1]++){
	for(j[0]=mins[0];j[0]<=maxs[0];j[0]++){
	  integral+=histo->GetBinContent(j[0],j[1],j[2]);
	}
      }
    }
    integral*=binvol;

  //Else if bin width is not constant for some direction
  } else {

    //If integrating using bin width for all axes
    if(!widths) {
      for(j[2]=mins[2];j[2]<=maxs[2];j[2]++){
	binwidth2=axes[2]->GetBinWidth(j[2]);
	for(j[1]=mins[1];j[1]<=maxs[1];j[1]++){
	  binvol=binwidth2*axes[1]->GetBinWidth(j[1]);
	  for(j[0]=mins[0];j[0]<=maxs[0];j[0]++){
	    integral+=histo->GetBinContent(j[0],j[1],j[2])*binvol*axes[0]->GetBinWidth(j[0]);
	  }
	}
      }

    //Else if integrating without using bin width at all
    } else if(!widths[0] && !widths[1] && !widths[2]) {
      for(j[2]=mins[2];j[2]<=maxs[2];j[2]++){
	for(j[1]=mins[1];j[1]<=maxs[1];j[1]++){
	  for(j[0]=mins[0];j[0]<=maxs[0];j[0]++){
	    integral+=histo->GetBinContent(j[0],j[1],j[2]);
	  }
	}
      }

    //Else if integrating using bin width for some of the directions only
    } else {

      for(j[2]=mins[2];j[2]<=maxs[2];j[2]++){
	binwidth2=(widths[2]?axes[2]->GetBinWidth(j[2]):1);

	for(j[1]=mins[1];j[1]<=maxs[1];j[1]++){
	  binvol=(widths[1]?binwidth2*axes[1]->GetBinWidth(j[1]):binwidth2);

	  for(j[0]=mins[0];j[0]<=maxs[0];j[0]++){
	    integral+=histo->GetBinContent(j[0],j[1],j[2])*(widths[0]?binvol*axes[0]->GetBinWidth(j[0]):binvol);
	  }
	}
      }
    }
  }

  delete[] axes;

  return integral;
}

Double_t QDisTH::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const
{
 //This function returns the probability density associated with a point which
 //coordinates are (x,y,z). For p.d.f. with less than 3 dimensions, the
 //arguments of extra dimensions are optional. Before calling this function,
 //the user must call QDisTH::Normalize() to normalize the p.d.f. properly. 

  PRINTF2(this,"\tDouble_t QDisTH::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const\n")  

  try{
    
  TH1* histo=dynamic_cast<TH1*>(GetObject());//Histogram pointer

  return (Double_t)histo->GetBinContent((histo->GetXaxis())->FindFixBin(x),
      (histo->GetYaxis())->FindFixBin(y),(histo->GetZaxis())->FindFixBin(z));

  } catch (Int_t i){
    cout << "Exception handled by QDisTH1D::ProbDensity\n";
    throw i;
  }    
}

QDisTH* QDisTH::MarginalPDF(const char *name, Int_t xaxis, Int_t yaxis) const
{
  TH1* histo=dynamic_cast<TH1*>(GetObject());//Histogram pointer

  Int_t dim=histo->GetDimension(); //PDF dimension

  if(dim==1 || (yaxis==-1 && dim<3)) return NULL;

  if(xaxis<0 || xaxis>=dim) {
    fprintf(stderr,"QDisTH::MarginalPDF: Error: Invalid axis index\n");
    throw 1;
  }

  if(yaxis<-1 || yaxis>=dim) {
    fprintf(stderr,"QDisTH::MarginalPDF: Error: Invalid axis index\n");
    throw 1;
  }
  TAxis **axes=new TAxis*[dim];
  QDisTH *th;

  axes[0]=histo->GetXaxis();
  if(dim>1) axes[1]=histo->GetYaxis();
  if(dim>2) axes[2]=histo->GetZaxis();

  if(yaxis==-1) {

    if(!axes[xaxis]->GetXbins()->fN) {
      th=new QDisTH(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXmin(),axes[xaxis]->GetXmax());

    } else {
      th=new QDisTH(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXbins()->GetArray());
    }

  } else {

    if(!axes[xaxis]->GetXbins()->fN) {

      if(!axes[yaxis]->GetXbins()->fN) {
      th=new QDisTH(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXmin(),axes[xaxis]->GetXmax(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXmin(),axes[yaxis]->GetXmax());

      } else {
	th=new QDisTH(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXmin(),axes[xaxis]->GetXmax(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXbins()->GetArray());
      }

    } else {

      if(!axes[yaxis]->GetXbins()->fN) {
	th=new QDisTH(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXbins()->GetArray(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXmin(),axes[yaxis]->GetXmax());

      } else {
	th=new QDisTH(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXbins()->GetArray(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXbins()->GetArray());
      }
    }
  }

  Int_t i,j;
  Bool_t widths[3];
  Int_t* binranges[3];

  //Initialize binranges pointers to NULL
  memset(binranges,0,3*sizeof(Int_t*));

  binranges[xaxis]=new Int_t[2];
  if(yaxis!=-1) binranges[yaxis]=new Int_t[2];

  widths[0]=!(fNormFlags&kNoXBinWidthNorm);
  widths[1]=!(fNormFlags&kNoYBinWidthNorm);
  widths[2]=!(fNormFlags&kNoZBinWidthNorm);
  widths[xaxis]=kFALSE;
  if(yaxis!=-1) widths[yaxis]=kFALSE;

  if(yaxis==-1) {

    for(i=1; i<=axes[xaxis]->GetNbins(); i++) { 
      binranges[xaxis][0]=binranges[xaxis][1]=i;
      ((TH1&)*th).SetBinContent(i,Integral(binranges,widths));
    }

  } else {

    for(i=1; i<=axes[xaxis]->GetNbins(); i++) { 
      binranges[xaxis][0]=binranges[xaxis][1]=i;

      for(j=1; j<=axes[yaxis]->GetNbins(); j++) { 
	binranges[yaxis][0]=binranges[yaxis][1]=j;
	((TH1&)*th).SetBinContent(i,j,Integral(binranges,widths));
      }
    }
  }

  delete[] binranges[xaxis];
  delete[] axes;

  return th;
}

void QDisTH::Normalize(Double_t* integral)
{
 //This function normalizes the PDF according to the normalization
 //flags sets by SetNormFlags. This
 //string is a standard ROOT selection expression that contains x
 //and/or y and/or z variables.

 PRINTF4(this,"\tvoid QDisTH::Normalize(Double_t* integral<",integral,">)\n")

   try{

     if(!(fNormFlags&kNoNorm)) {
       TH1* histo;                 //Histogram pointer
       Double_t cutintbuf;         //Buffer for integral value(s)
       Int_t nfix=fNormFlags&3;    //Number of fixed coordinates for a conditional PDF
       Bool_t *widths=NULL;

       //If not using bin width for normalization for at least one direction
       if(fNormFlags&kNoBinWidthNorm) {
	 widths=new Bool_t[3];
	 widths[0]=!(fNormFlags&kNoXBinWidthNorm);
	 widths[1]=!(fNormFlags&kNoYBinWidthNorm);
	 widths[2]=!(fNormFlags&kNoZBinWidthNorm);
       }

       histo=dynamic_cast<TH1*>(GetObject());

       Int_t dim=histo->GetDimension(); //PDF dimension
       Int_t nbins[3];                  //Number of bins for each dimension
       Int_t biniter[3];                //Integers used as indices for iteration
       Int_t bin;                       //bin index buffer
       Double_t scale=histo->GetEntries(); //scaling factor used for normalization of histograms filled with number of events

       //Get the number of bins for each dimension
       nbins[0]=histo->GetNbinsX();
       nbins[1]=(dim>1 ? histo->GetNbinsY() : 1);
       nbins[2]=(dim>2 ? histo->GetNbinsZ() : 1);

       //Normalization of histograms with variable size for which the bin content corresponds to a number of events
       if(fNormFlags&(kEventsFilled|kVarBinSizeEventsFilled)){
	 TAxis *vbsaxis[3];                  //array of axis using variable bin width
	 Double_t binvol, binwidth2;         //buffer for variable bin width/area/volume
	 memset(vbsaxis,0,3*sizeof(TAxis*)); //Initialize axis pointers to NULL

	 //Add TAxis pointers to vbsaxis for axis having variable bin width
	 if((histo->GetXaxis())->GetNbins()>1 && (histo->GetXaxis())->GetXbins()->fN) vbsaxis[0]=histo->GetXaxis();
	 if((histo->GetYaxis())->GetNbins()>1 && (histo->GetYaxis())->GetXbins()->fN) vbsaxis[1]=histo->GetYaxis();
	 if((histo->GetZaxis())->GetNbins()>1 && (histo->GetZaxis())->GetXbins()->fN) vbsaxis[2]=histo->GetZaxis();

	 if(!vbsaxis[0] && (!widths || widths[0])) scale*=histo->GetXaxis()->GetBinWidth(1);
	 if(!vbsaxis[1] && (!widths || widths[1])) scale*=histo->GetYaxis()->GetBinWidth(1);
	 if(!vbsaxis[2] && (!widths || widths[2])) scale*=histo->GetZaxis()->GetBinWidth(1);

	 if(vbsaxis[0] || vbsaxis[1] || vbsaxis[2]) {

	   //Loop over all bins
	   for(biniter[2]=1;biniter[2]<=nbins[2];biniter[2]++){
	     binwidth2=(vbsaxis[2]?vbsaxis[2]->GetBinWidth(biniter[2]):1);

	     for(biniter[1]=1;biniter[1]<=nbins[1];biniter[1]++){
	       binvol=(vbsaxis[1]?binwidth2*vbsaxis[1]->GetBinWidth(biniter[1]):binwidth2);

	       for(biniter[0]=1;biniter[0]<=nbins[0];biniter[0]++){
		 //Get the bin index
		 bin=histo->GetBin(biniter[0],biniter[1],biniter[2]);
		 //Scale the bin value
		 histo->SetBinContent(bin,histo->GetBinContent(bin)/(vbsaxis[0]?binvol*vbsaxis[0]->GetBinWidth(biniter[0]):binvol));
	       }
	     }
	   }
	 }
       }

       //If the histogram has to be normalized as a conditional PDF
       if(nfix){
	 Int_t fcoord,coord;              //Index of the first fixed coordinate
	 Int_t* binranges[3];             //Bin indices (for fixed coordinates)
	 Double_t scutintbuf;             //Integral value and error buffers

	 //If the number of fix dimensions is greater than the total number of
	 //dimensions, set the number of fix dimensions to the total number of
	 //dimensions
	 if(nfix>dim) nfix=dim;

	 cutintbuf=0;

	 //Initialized the binranges pointers to NULL
	 memset(binranges,0,3*sizeof(Int_t*));


	 if(!widths) {
	   widths=new Bool_t[3];
	   //Loop over non-fixed dimensions
	   for(fcoord=0; fcoord<dim-nfix; fcoord++) widths[fcoord]=kTRUE;
	 }

	 //Loop over the fixed dimensions
	 for(fcoord=dim-nfix; fcoord<3; fcoord++){
	   //Allocate memory for the current fixed dimension
	   binranges[fcoord]=new Int_t[2];
	   //Initialize the binrange for the current fixed dimension to 1
	   *(binranges[fcoord]+1)=*(binranges[fcoord])=1;
	   widths[fcoord]=kFALSE;
	 }

	 //Loop over the bin indices of fixed dimensions
	 do{
	   //Set fcoord to the index of the first fixed dimension
	   fcoord=dim-nfix;
	   //Compute the integral of the conditional PDF for a given fixed bin
	   scutintbuf=Integral(binranges,widths);
	   //Add the integral value to the total
	   cutintbuf+=scutintbuf;

	   //If the integral value is not 0
	   if(scutintbuf){
	     //Set all the biniter elements to 0
	     memset(biniter,0,3*sizeof(Int_t));

	     //Loop over the indices of the fixed variable
	     for(coord=dim-nfix;coord<3;coord++){
	       //Set the biniter elements that are associated to fixed variables
	       //to the current bin
	       biniter[coord]=*(binranges[coord]);
	     }

	     //Loop over bin indices of non-fixed dimensions
	     do{
	       coord=0;
	       //Get the bin index
	       bin=histo->GetBin(biniter[0],biniter[1],biniter[2]);
	       //Scale the bin value
	       histo->SetBinContent(bin,histo->GetBinContent(bin)/scutintbuf);
	       biniter[coord]++;

	       while(biniter[coord]>nbins[coord]+1){
		 biniter[coord]=0;
		 coord++;

		 if(coord>=dim-nfix) break;
		 biniter[coord]++;
	       }
	     } while(coord<dim-nfix);
	   }
	   *(binranges[fcoord]+1)=++*(binranges[fcoord]);

	   while(*(binranges[fcoord])>nbins[fcoord]){
	     *(binranges[fcoord]+1)=*(binranges[fcoord])=1;
	     fcoord++;

	     if(fcoord>=dim) break;
	     *(binranges[fcoord]+1)=++*(binranges[fcoord]);
	   }
	 } while(fcoord<dim);

	 //Loop over the fixed dimensions
	 for(fcoord=dim-nfix; fcoord<3; fcoord++){
	   //Delete the array
	   delete[] binranges[fcoord];
	 }

	 //If histogram has not to be normalized as a conditional PDF
       } else {

	 if(fNormFlags&kEventsFilled) {
	   cutintbuf=scale;

	 } else {
	   //Compute the integral of the histogram
	   cutintbuf=Integral(NULL,widths);
	 }

	 //If the integral value is not 0, normalize the PDF
	 if (cutintbuf) histo->Scale(1/cutintbuf);
       }

       if(integral) *integral=cutintbuf;
       if(widths) delete[] widths;
     }
   }catch(Int_t e){
    cout << "Exception handled by QDisTH::Normalize\n";
    throw e;
  }
}

#include "debugger.h"
