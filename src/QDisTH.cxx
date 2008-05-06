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

QTHOps QDisTH::fQTHOps; //fQTHOps will perform operations on the pdf

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, Float_t ylow, Float_t yhigh, Int_t nbinsz, Float_t zlow, Float_t zhigh)
{

  if(nbinsy=0) {
    SetObject("TH1F",new TH1F(name,title,nbinsx,xlow,xhigh));

  } else if(nbinsz=0) {
    SetObject("TH2F",new TH2F(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh));

  } else {
    SetObject("TH3F",new TH3F(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh.nbinsz,zlow,zhigh));
  }
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t *xbins, Int_t nbinsy, Float_t *ybins, Int_t nbinsz, Float_t *zbins)
{

  if(nbinsy=0) {
    SetObject("TH1F",new TH1F(name,title,nbinsx,xbins));

  } else if(nbinsz=0) {
    SetObject("TH2F",new TH2F(name,title,nbinsx,xbins,nbinsy,ybins));

  } else {
    SetObject("TH3F",new TH3F(name,title,nbinsx,xbins,nbinsy,ybins.nbinsz,zbins));
  }
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, Double_t ylow, Double_t yhigh, Int_t nbinsz, Double_t zlow, Double_t zhigh)
{

  if(nbinsy=0) {
    SetObject("TH1D",new TH1D(name,title,nbinsx,xlow,xhigh));

  } else if(nbinsz=0) {
    SetObject("TH2D",new TH2D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh));

  } else {
    SetObject("TH3D",new TH3D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh.nbinsz,zlow,zhigh));
  }
}

QDisTH::QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t *xbins, Int_t nbinsy, Double_t *ybins, Int_t nbinsz, Double_t *zbins)
{

  if(nbinsy=0) {
    SetObject("TH1D",new TH1D(name,title,nbinsx,xlow,xhigh));

  } else if(nbinsz=0) {
    SetObject("TH2D",new TH2D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh));

  } else {
    SetObject("TH3D",new TH3D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh.nbinsz,zlow,zhigh));
  }
}

Int_t Fill(Double_t x, Double_t y, Double_t z)
{
  switch(GetDimension()) {
    case 1:
      return dynamic_cast<TH1*>(fObject)->Fill(x);
    case 2:
      return dynamic_cast<TH2*>(fObject)->Fill(y);
    case 3:
      return dynamic_cast<TH3*>(fObject)->Fill(z);
  }
  return -1;
}

Double_t QDisTH::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const
{
 //This function returns the probability density associated with a point which
 //coordinates are (x,y,z). For p.d.f. with less than 3 dimensions, the
 //arguments of extra dimensions are optional. Before calling this function,
 //the user must call QDisTH::Normalize() to normalize the p.d.f. properly. 

  PRINTF2(this,"\tDouble_t QDisTH::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const\n")  

  try{
    
    fQTHOps.SetTH(dynamic_cast<TH1*>(GetObject())); //Set the pdf.  GetObject() is a QTObjectIO method.

    return fQTHOps.Freq(x,y,z);
    
  } catch (Int_t i){
    cout << "Exception handled by QDisTH1D::ProbDensity\n";
    throw i;
  }    
}

void QDisTH::Normalize(Option_t* cutexpr, Int_t normflags, Double_t* fullintegral, Double_t* cutintegral, Double_t* error)
{
 //This function normalizes the PDF according to the normalization
 //flags normflags and the cuts defined via the cutexpr string. This
 //string is a standard ROOT selection expression that contains x
 //and/or y and/or z variables. The normalization flag values are the
 //following:
 // 
 //0: Regular PDF 
 //1: Conditional PDF with 1 fixed coordinate
 //2: Conditional PDF with 2 fixed coordinates
 //3: Conditional PDF with 3 fixed coordinates
 //4: PDF with variable bin size for which the histogram bin content
 //   corresponds to a number of events
 //
 //Flags can be combined using a bitwise "OR" operator
 //
 //Since a histogram is a binned function, there is a normalization
 //error when the cuts cross bins.  If the fourth argument is provided
 //by the user, it is filled with the normalization error. The
 //cutintegral value is computed by including all the bins for which
 //at least one corner pass the cuts.  The error is the total area,
 //volume or hypervolume occupied by the bins that cross the cuts. The
 //second and third arguments (optional) are filled with the total
 //function integral and the integral of function between cuts
 //respectively.

  PRINTF12(this,"\tvoid QDisTH::Normalize(Option_t* cutexpr<",cutexpr,">, Int_t normflags<",normflags,">, Double_t* fullintegral<",fullintegral,">, Double_t* cutintegral<",cutintegral,">, Double_t* error<",error,">)\n")

  try{
    TH1* histo;                 //Histogram pointer
    Double_t cutintbuf;         //Buffer for integral value(s)
    Int_t nfix=normflags&3;     //Number of fixed coordinates for a conditional PDF

    histo=dynamic_cast<TH1*>(GetObject());
    
    Int_t dim=histo->GetDimension(); //PDF dimension
    Int_t nbins[3];                  //Number of bins for each dimension
    Int_t biniter[3];                //Integers used as indices for iteration
    Int_t bin;                       //bin index buffer

    //Get the number of bins for each dimension
    nbins[0]=histo->GetNbinsX();
    nbins[1]=(dim>1 ? histo->GetNbinsY() : 1);
    nbins[2]=(dim>2 ? histo->GetNbinsZ() : 1);

    //Normalization of histograms with variable size for which the bin content corresponds to a number of events
    if(normflags&4){
      TAxis *vbsaxis[3];                  //array of axis using variable bin width
      Int_t i;                            //iterator
      Double_t binwidth;                  //buffer for variable bin width/area/volume
      memset(vbsaxis,0,3*sizeof(TAxis*)); //Initialize axis pointers to NULL

      //Add TAxis pointers to vbsaxis for axis having variable bin width
      if((histo->GetXaxis())->GetNbins()>1 && (histo->GetXaxis())->GetXbins()->fN) vbsaxis[0]=histo->GetXaxis();
      if((histo->GetYaxis())->GetNbins()>1 && (histo->GetYaxis())->GetXbins()->fN) vbsaxis[1]=histo->GetYaxis();
      if((histo->GetZaxis())->GetNbins()>1 && (histo->GetZaxis())->GetXbins()->fN) vbsaxis[2]=histo->GetZaxis();

      //Loop over all bins
      for(biniter[0]=1;biniter[0]<=nbins[0];biniter[0]++){

	for(biniter[1]=1;biniter[1]<=nbins[1];biniter[1]++){

	  for(biniter[2]=1;biniter[2]<=nbins[2];biniter[2]++){
	    binwidth=1;
	    i=0;

	    //Compute the bin width/area/volume using axis having variable bin width
	    while(i<dim && binwidth){
	      if(vbsaxis[i]) binwidth*=vbsaxis[i]->GetBinWidth(biniter[i]);
	      i++;
	    }

	    //If computer bin width is not 0
	    if(binwidth){
	      //Get the bin index
	      bin=histo->GetBin(biniter[0],biniter[1],biniter[2]);
	      //Scale the bin value
	      histo->SetBinContent(bin,histo->GetBinContent(bin)/binwidth);
	    }
	  }
	}
      }
    }

    fQTHOps.SetTH(histo);  //set the pdf.  GetObject is a QTObjectIO method.

    //Compute the full integral of the histogram
    if(fullintegral) *fullintegral=fQTHOps.BinIntegral();

    //If the histogram has to be normalized as a conditional PDF
    if(nfix){
      Int_t fcoord,coord;              //Index of the first fixed coordinate
      Int_t* binranges[3];             //Bin indices (for fixed coordinates)
      Double_t scutintbuf, serror;     //Integral value and error buffers

      //If the number of fix dimensions is greater than the total number of
      //dimensions, set the number of fix dimensions to the total number of
      //dimensions
      if(nfix>dim) nfix=dim;

      cutintbuf=0;

      if(error) *error=0;
      //Initialized the binranges pointers to NULL
      memset(binranges,0,3*sizeof(Int_t*));

      //Loop over the fixed dimensions
      for(fcoord=dim-nfix; fcoord<3; fcoord++){
	//Allocate memory for the current fixed dimension
	binranges[fcoord]=new Int_t[2];
	//Initialize the binrange for the current fixed dimension to 1
	*(binranges[fcoord]+1)=*(binranges[fcoord])=1;
      }

      //Loop over the bin indices of fixed dimensions
      do{
	//Set fcoord to the index of the first fixed dimension
	fcoord=dim-nfix;
	//Compute the integral of the conditional PDF for a given fixed bin
	scutintbuf=fQTHOps.LimIntegral(cutexpr,&serror,binranges);
	//Add the integral value to the total
	cutintbuf+=scutintbuf;

	//Add the error to the total
	if(error) *error+=serror;

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
      //Compute the integral of the histogram
      cutintbuf=fQTHOps.LimIntegral(cutexpr,error);

      //If the integral value is not 0, normalize the PDF
      if (cutintbuf) histo->Scale(1/cutintbuf);
    }

    if(cutintegral) *cutintegral=cutintbuf;

  }catch(Int_t e){
    cout << "Exception handled by QDisTH::Normalize\n";
    throw e;
  }
}

#include "debugger.h"
