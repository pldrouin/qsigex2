// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QDisTHN.h"

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTHN                                                          //
//                                                                  //
// This class creates a probability density function from a TH      //
// object. The class is derived from abstract base class QDis       //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

template <typename U> Int_t QDisTHN<U>::Fill(const Double_t &x)
{
  if(fQTHN->GetNDims()!=1) {
    fprintf(stderr,"QDisTHN::Fill: Error: Histogram dimension is not 1\n");
    throw 1;
  }
  return fQTHN->Fill(&x);
}

template <typename U> Int_t QDisTHN<U>::Fill(const Double_t &x, const Double_t &y)
{
  if(fQTHN->GetNDims()!=2) {
    fprintf(stderr,"QDisTHN::Fill: Error: Histogram dimension is not 2\n");
    throw 1;
  }
  const Double_t xs[2]={x,y};
  return fQTHN->Fill(xs);
}

template <typename U> Int_t QDisTHN<U>::Fill(const Double_t &x, const Double_t &y, const Double_t &z)
{
  if(fQTHN->GetNDims()!=3) {
    fprintf(stderr,"QDisTHN::Fill: Error: Histogram dimension is not 3\n");
    throw 1;
  }
  const Double_t xs[3]={x,y,z};
  return fQTHN->Fill(xs);
}

template <typename U> Int_t QDisTHN<U>::Fill(const Double_t *x)
{
  return fQTHN->Fill(x);
}

template <typename U> Double_t QDisTHN<U>::Integral(Int_t** binranges, Bool_t *widths) const
{
  const Int_t dim=fQTHN->GetNDims();
  Int_t mins[dim], maxs[dim];
  Int_t i, j[dim];
  Long64_t li;
  TAxis *axes[dim];
  Bool_t cbw=kTRUE; //Constant bin width in all directions
  Bool_t bwi=kFALSE;//Integration using bin width for at least some direction

  for(i=0;i<dim;i++){
    axes[i]=fQTHN->GetAxis(i);

    if(binranges && binranges[i]){
      mins[i]=(*(binranges[i])>1)?*(binranges[i]):1;
      maxs[i]=(*(binranges[i]+1)<axes[i]->GetNbins())?*(binranges[i]+1):axes[i]->GetNbins(); 
    } else {
      mins[i]=1;
      maxs[i]=axes[i]->GetNbins();
    }
    if(axes[i]->GetXbins()->fN) cbw=kFALSE;
    if(!widths || widths[i]) bwi=kTRUE;
    //cout << mins[i] << "\t" << maxs[i] << "\n";
  }

  Double_t integral=0;
  Double_t binvol;

  //If bin width is constant in all directions
  if(cbw) {

    //If integrating using bin width for all axes
    if(!widths) {
      binvol=axes[0]->GetBinWidth(1);
      for(i=1; i<dim; i++) binvol*=axes[i]->GetBinWidth(1);

      //Else if using bin width for only some axes
    } else {
      binvol=1;
      for(i=0; i<dim; i++) if(widths[i]) binvol*=axes[i]->GetBinWidth(1);
    }

    for(li=0; li<fQTHN->GetNFbins(); li++) {
      if(fQTHN->IsBinIncluded(li,mins,maxs)) integral+=fQTHN->GetFBinContent(li);
    }
    integral*=binvol;

  //Else if bin width is not constant for some direction
  } else {

    //If integrating using bin width for all axes
    if(!widths) {

      for(li=0; li<fQTHN->GetNFbins(); li++) {
	fQTHN->GetBinCoords(li,j);

	for(i=0; i<dim; i++) if(j[i]<mins[i] || j[i]>maxs[i]) break; 

	if(i==dim) {
	  binvol=axes[0]->GetBinWidth(j[0]);
	  for(i=1; i<dim; i++) binvol*=axes[i]->GetBinWidth(j[i]);
	  integral+=fQTHN->GetFBinContent(li)*binvol;
	}
      }

      //Else if integrating without using bin width at all
    } else if(!bwi) {

      for(li=0; li<fQTHN->GetNFbins(); li++) {
	if(fQTHN->IsBinIncluded(li,mins,maxs)) integral+=fQTHN->GetFBinContent(li);
      }

      //Else if integrating using bin width for some of the directions only
    } else {

      for(li=0; li<fQTHN->GetNFbins(); li++) {
	fQTHN->GetBinCoords(li,j);

	for(i=0; i<dim; i++) if(j[i]<mins[i] || j[i]>maxs[i]) break; 

	if(i==dim) {
	  binvol=(widths[0]?axes[0]->GetBinWidth(j[0]):1);
	  for(i=1; i<dim; i++) binvol*=(widths[i]?axes[i]->GetBinWidth(j[i]):1);
	  integral+=fQTHN->GetFBinContent(li)*binvol;
	}
      }
    }
  }
  return integral;
}

template <typename U> Double_t QDisTHN<U>::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const
{
 //This function returns the probability density associated with a point which
 //coordinates are (x,y,z). For p.d.f. with less than 3 dimensions, the
 //arguments of extra dimensions are optional. Before calling this function,
 //the user must call QDisTHN<U>::Normalize() to normalize the p.d.f. properly. 

 if(fQTHN->GetNDims()>3) {
    fprintf(stderr,"QDisTHN::Fill: Error: Histogram dimension is greater than 3.\n");
    throw 1;
 }

 switch(fQTHN->GetNDims()) {
   case 1:
     return fQTHN->GetBinContent(fQTHN->FindBin(&x));
   case 2:
     {
       const Double_t xs[2]={x,y};
       return fQTHN->GetBinContent(fQTHN->FindBin(xs));
     }
   case 3:
     {
       const Double_t xs[3]={x,y,z};
       return fQTHN->GetBinContent(fQTHN->FindBin(xs));
     }
 }
 return 0;
}

template <typename U> QDisTHN<U>* QDisTHN<U>::MarginalPDF(const char *name, Int_t xaxis, Int_t yaxis) const
{
/*  Int_t dim=fQTHN->GetDimension(); //PDF dimension

  if(dim==1 || (yaxis==-1 && dim<3)) return NULL;

  if(xaxis<0 || xaxis>=dim) {
    fprintf(stderr,"QDisTHN<U>::MarginalPDF: Error: Invalid axis index\n");
    throw 1;
  }

  if(yaxis<-1 || yaxis>=dim) {
    fprintf(stderr,"QDisTHN<U>::MarginalPDF: Error: Invalid axis index\n");
    throw 1;
  }
  TAxis **axes=new TAxis*[dim];
  QDisTHN *th;

  axes[0]=fQTHN->GetXaxis();
  if(dim>1) axes[1]=fQTHN->GetYaxis();
  if(dim>2) axes[2]=fQTHN->GetZaxis();

  if(yaxis==-1) {

    if(!axes[xaxis]->GetXbins()->fN) {
      th=new QDisTHN(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXmin(),axes[xaxis]->GetXmax());

    } else {
      th=new QDisTHN(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXbins()->GetArray());
    }

  } else {

    if(!axes[xaxis]->GetXbins()->fN) {

      if(!axes[yaxis]->GetXbins()->fN) {
      th=new QDisTHN(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXmin(),axes[xaxis]->GetXmax(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXmin(),axes[yaxis]->GetXmax());

      } else {
	th=new QDisTHN(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXmin(),axes[xaxis]->GetXmax(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXbins()->GetArray());
      }

    } else {

      if(!axes[yaxis]->GetXbins()->fN) {
	th=new QDisTHN(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXbins()->GetArray(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXmin(),axes[yaxis]->GetXmax());

      } else {
	th=new QDisTHN(name,name,axes[xaxis]->GetNbins(),axes[xaxis]->GetXbins()->GetArray(),axes[yaxis]->GetNbins(),axes[yaxis]->GetXbins()->GetArray());
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
  */
}

template <typename U> void QDisTHN<U>::Normalize(Double_t* integral)
{
  //This function normalizes the PDF according to the normalization
  //flags sets by SetNormFlags. This
  //string is a standard ROOT selection expression that contains x
  //and/or y and/or z variables.

  if(!(fNormFlags&kNoNorm)) {
    Double_t cutintbuf;         //Buffer for integral value(s)
    Int_t nfix=fNFixedCoords;   //Number of fixed coordinates for a conditional PDF
    Bool_t *widths=NULL;
    Int_t dim=GetDimension();   //PDF dimension

    //If not using bin width for normalization for at least one direction
    if(fNormFlags&kNoBinWidthNorm) {
      widths=new Bool_t[dim];
      memset(widths,0,dim*sizeof(Bool_t));
    }

    Int_t *nbins=new Int_t[dim];     //Number of bins for each dimension
    Int_t *biniter=new Int_t[dim];   //Integers used as indices for iteration
    Int_t i;
    Double_t scale=fQTHN->GetEntries(); //scaling factor used for normalization of histograms filled with number of events

    //Get the number of bins for each dimension
    for(i=0; i<dim; i++) nbins[i]=fQTHN->GetAxis(i)->GetNbins();

    //Normalization of histograms with variable size for which the bin content corresponds to a number of events
    if(fNormFlags&(kEventsFilled|kVarBinSizeEventsFilled)){
      TAxis **vbsaxis=new TAxis*[dim];      //array of axis using variable bin width
      Double_t binvol;                      //buffer for variable bin width/area/volume
      memset(vbsaxis,0,dim*sizeof(TAxis*)); //Initialize axis pointers to NULL
      Bool_t hasvbaxes=kFALSE;

      for(i=0; i<dim; i++) {
        //Add TAxis pointers to vbsaxis for axis having variable bin width

	if(fQTHN->GetAxis(i)->GetNbins()>1 && fQTHN->GetAxis(i)->GetXbins()->fN) {
	  vbsaxis[i]=fQTHN->GetAxis(i);
	  hasvbaxes=kTRUE;
	} else if(!widths || widths[i]) scale*=fQTHN->GetAxis(i)->GetBinWidth(1);
      }

      if(hasvbaxes) {
	Long64_t li;

	for(li=0; li<fQTHN->GetNFbins(); li++) {
	  fQTHN->GetBinCoords(li,biniter);
	  binvol=(vbsaxis[0]?vbsaxis[0]->GetBinWidth(biniter[0]):1);
	  for(i=1; i<dim; i++) if(vbsaxis[i]) binvol*=vbsaxis[i]->GetBinWidth(biniter[i]);
	  fQTHN->SetFBinContent(li,fQTHN->GetFBinContent(li)/binvol);
	}
      }
      delete[] vbsaxis;
    }

    //If the histogram has to be normalized as a conditional PDF
    if(nfix){
      Int_t fcoord,coord;                //Index of the first fixed coordinate
      Int_t **binranges=new Int_t*[dim]; //Bin indices (for fixed coordinates)
      Double_t scutintbuf;               //Integral value and error buffers

      //If the number of fix dimensions is greater than the total number of
      //dimensions, set the number of fix dimensions to the total number of
      //dimensions
      if(nfix>dim) nfix=dim;

      cutintbuf=0;

      //Initialized the binranges pointers to NULL
      memset(binranges,0,dim*sizeof(Int_t*));


      if(!widths) {
	widths=new Bool_t[dim];
	//Loop over non-fixed dimensions
	for(fcoord=0; fcoord<dim-nfix; fcoord++) widths[fcoord]=kTRUE;
      }

      //Loop over the fixed dimensions
      for(fcoord=dim-nfix; fcoord<dim; fcoord++){
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
	  memset(biniter,0,dim*sizeof(Int_t));

	  //Loop over the indices of the fixed variable
	  for(coord=dim-nfix;coord<dim;coord++){
	    //Set the biniter elements that are associated to fixed variables
	    //to the current bin
	    biniter[coord]=*(binranges[coord]);
	  }

	  //Loop over bin indices of non-fixed dimensions
	  do{
	    coord=0;
	    //Scale the bin value
	    fQTHN->ScaleBinContent(biniter, 1./scutintbuf);
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
      for(fcoord=dim-nfix; fcoord<dim; fcoord++){
	//Delete the array
	delete[] binranges[fcoord];
      }
      delete[] binranges;

      //If histogram has not to be normalized as a conditional PDF
    } else {

      if(fNormFlags&kEventsFilled) {
	cutintbuf=scale;

      } else {
	//Compute the integral of the histogram
	cutintbuf=Integral(NULL,widths);
      }

      //If the integral value is not 0, normalize the PDF
      if (cutintbuf) fQTHN->Scale(1/cutintbuf);
    }

    if(integral) *integral=cutintbuf;
    if(widths) delete[] widths;
    delete[] nbins;
    delete[] biniter;
  }
}
