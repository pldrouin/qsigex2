// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QDisTHN.h"

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTHN                                                          //
//                                                                  //
// This class creates a probability density function from a THN     //
// object. The class is derived from abstract base class QDis       //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &ndims, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,ndims);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,ndims);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,ndims);
  }
  SetNameTitleToObject();
}

/*
template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh);
  }
  SetNameTitleToObject();
}

template <typename U> QDisTHN<U>::QDisTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Int_t &qthntype): QDis(), fOwned(kTRUE), fQTHN(NULL)
{
  switch(qthntype) {
    case kQTHN:
      fQTHN=new QTHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNF:
      fQTHN=new QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins);
      break;

    case kQTHNDL:
      fQTHN=new QTHNDL<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins);
  }
  SetNameTitleToObject();
}
*/


template <typename U> Double_t QDisTHN<U>::ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const
{
  //This function returns the probability density associated with a point which
  //coordinates are (x,y,z). For p.d.f. with less than 3 dimensions, the
  //arguments of extra dimensions are optional. Before calling this function,
  //the user must call QDisTHN<U>::Normalize() to normalize the p.d.f. properly. 

  switch(fQTHN->GetNDims()) {
    case 1:
      return fQTHN->GetBinContent(fQTHN->FindBin(x));
    case 2:
      return fQTHN->GetBinContent(fQTHN->FindBin(x,y));
    case 3:
      return fQTHN->GetBinContent(fQTHN->FindBin(x,y,z));
  }
  return 0;
}

template <typename U> QDisTHN<U>* QDisTHN<U>::MarginalPDF(const char *name, const Int_t *axes, const Int_t &naxes) const
{
  Int_t dim=fQTHN->GetNDims(); //PDF dimension
  Int_t nfix=(Int_t)fNFixedCoords>dim?dim:(Int_t)fNFixedCoords; //Number of fixed coordinates for a conditional PDF

  if(naxes<=0 || !axes || naxes>=dim-nfix) return NULL;

  Int_t i;

  for(i=0; i<naxes; i++) {
    if(axes[i]<0 || axes[i]>=dim-nfix) {
      fprintf(stderr,"QDisTHN<U>::MarginalPDF: Error: Invalid axis index: %i\n",axes[i]);
      throw 1;
    }
  }

  Int_t naaxes=naxes+nfix;
  Int_t *aaxes=new Int_t[naaxes];
  memcpy(aaxes,axes,naxes*sizeof(Int_t));

  for(i=0; i<nfix; i++) {
    aaxes[naxes+i]=i+dim-nfix;
  }

  QAxis **taxes=new QAxis*[dim];
  QDisTHN<U> *th;

  for(i=0; i<dim; i++) taxes[i]=fQTHN->GetAxis(i);

  Int_t type=kQTHN;

  if(dynamic_cast<QTHNF<U>*>(fQTHN)) type=kQTHNF;
  else if(dynamic_cast<QTHNDL<U>*>(fQTHN)) type=kQTHNDL;

  th=new QDisTHN<U>(name,name,naaxes,type);
  th->SetNormFlags(GetNormFlags()&~(QDis::kEventsFilled|QDis::kVarBinSizeEventsFilled));
  th->SetNFixedCoords(GetNFixedCoords());

  Bool_t *widths=new Bool_t[dim];
  Int_t **binranges=new Int_t*[dim];
  Int_t *biniter=new Int_t[naaxes];    //Integers used as indices for iteration

  //Initialize binranges pointers to NULL
  memset(binranges,0,dim*sizeof(Int_t*));

  for(i=0; i<dim; i++) widths[i]=kTRUE;

  for(i=0; i<naaxes; i++) {
    th->SetAxis(i,taxes[aaxes[i]]);
    binranges[aaxes[i]]=new Int_t[2];
    widths[aaxes[i]]=kFALSE;
    biniter[i]=1;
  }

  //Loop over bin indices of projection axes
  do{

    for(i=0; i<naaxes; i++) binranges[aaxes[i]][0]=binranges[aaxes[i]][1]=biniter[i];
    //Scale the bin value
    th->fQTHN->SetBinContent(biniter,fQTHN->Integral(binranges,widths));
    biniter[0]++;

    i=0;
    while(biniter[i]>taxes[aaxes[i]]->GetNBins()){
      biniter[i]=1;
      i++;

      if(i>=naaxes) break;
      biniter[i]++;
    }
  } while(i<naaxes);
  th->fQTHN->SetEntries(fQTHN->GetEntries());

  delete[] widths;

  for(i=0; i<naaxes; i++) delete[] binranges[aaxes[i]];
  delete[] binranges;
  delete[] biniter;
  delete[] taxes;
  delete[] aaxes;

  return th;
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
    Int_t dim=fQTHN->GetNDims(); //PDF dimension

    //If not using bin width for normalization for all directions
    if(fNormFlags&kNoBinWidthNorm) {
      widths=new Bool_t[dim];
      memset(widths,0,dim*sizeof(Bool_t));
    }

    Int_t *nbins=new Int_t[dim];     //Number of bins for each dimension
    Int_t *biniter=new Int_t[dim];   //Integers used as indices for iteration
    Int_t i;
    Double_t scale=fQTHN->GetEntries(); //scaling factor used for normalization of histograms filled with number of events

    //Get the number of bins for each dimension
    for(i=dim-1; i>=0; --i) nbins[i]=fQTHN->GetAxis(i)->GetNBins();

    //Normalization of histograms with variable size for which the bin content corresponds to a number of events
    if(fNormFlags&(kEventsFilled|kVarBinSizeEventsFilled)){
      QAxis **vbsaxis=new QAxis*[dim];      //array of axis using variable bin width
      Double_t binvol;                      //buffer for variable bin width/area/volume
      memset(vbsaxis,0,dim*sizeof(QAxis*)); //Initialize axis pointers to NULL
      Bool_t hasvbaxes=kFALSE;

      for(i=dim-1; i>=0; --i) {
	//Add QAxis pointers to vbsaxis for axis having variable bin width

	if(fQTHN->GetAxis(i)->GetNBins()>1 && fQTHN->GetAxis(i)->GetBins()) {
	  vbsaxis[i]=fQTHN->GetAxis(i);
	  hasvbaxes=kTRUE;
	} else if(!widths || widths[i]) scale*=fQTHN->GetAxis(i)->GetBinWidth(1);
      }

      if(hasvbaxes && !(fNormFlags&kNoBinWidthNorm)) {
	Long64_t li;

	for(li=fQTHN->GetNFbins()-1; li>=0; --li) {
	  fQTHN->GetFBinCoords(li,biniter);
	  binvol=(vbsaxis[0]?vbsaxis[0]->GetBinWidth(biniter[0]):1);
	  for(i=1; i<dim; ++i) if(vbsaxis[i]) binvol*=vbsaxis[i]->GetBinWidth(biniter[i]);
	  fQTHN->ScaleFBinContent(li,1/binvol);
	}
      }
      delete[] vbsaxis;
    }

    //If the histogram has to be normalized as a conditional PDF
    if(nfix){
      Int_t fcoord, coord;                //Index of the first fixed coordinate
      Int_t **binranges=new Int_t*[dim]; //Bin indices
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
	for(fcoord=dim-nfix-1; fcoord>=0; --fcoord) widths[fcoord]=kTRUE;
      }

      //Loop over the fixed dimensions
      for(fcoord=dim-nfix; fcoord<dim; ++fcoord){
	//Allocate memory for the current fixed dimension
	binranges[fcoord]=new Int_t[2];
	//Initialize the binrange for the current fixed dimension to 1
	*(binranges[fcoord]+1)=*(binranges[fcoord])=1;
	widths[fcoord]=kFALSE;
      }

      //Loop over the bin indices of fixed dimensions
      do{
	//Compute the integral of the conditional PDF for a given fixed bin
	scutintbuf=fQTHN->Integral(binranges,widths);
	//Add the integral value to the total
	cutintbuf+=scutintbuf;

	//If the integral value is not 0
	if(scutintbuf){
	  //Loop over the indices of non-fixed dimensions
	  for(coord=dim-nfix-1; coord>=0; --coord) biniter[coord]=1;

	  //Loop over the indices of fixed dimensions
	  for(coord=dim-nfix; coord<dim; ++coord){
	    //Set the biniter elements that are associated to fixed variables
	    //to the current bin
	    biniter[coord]=*(binranges[coord]);
	  }

	  //Loop over bin indices of non-fixed dimensions
	  do{
	    //Scale the bin value
	    fQTHN->ScaleBinContent(biniter, 1./scutintbuf);
	    biniter[0]++;

	    coord=0;
	    while(biniter[coord]>nbins[coord]){
	      biniter[coord]=1;
	      coord++;

	      if(coord>=dim-nfix) break;
	      biniter[coord]++;
	    }
	  } while(coord<dim-nfix);
	}
	//Set fcoord to the index of the first fixed dimension
	fcoord=dim-nfix;
	*(binranges[fcoord]+1)=++*(binranges[fcoord]);

	while(*(binranges[fcoord])>nbins[fcoord]){
	  *(binranges[fcoord]+1)=*(binranges[fcoord])=1;
	  fcoord++;

	  if(fcoord>=dim) break;
	  *(binranges[fcoord]+1)=++*(binranges[fcoord]);
	}
      } while(fcoord<dim);

      //Loop over the fixed dimensions
      for(fcoord=dim-nfix; fcoord<dim; ++fcoord){
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
	cutintbuf=fQTHN->Integral(NULL,widths);
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
