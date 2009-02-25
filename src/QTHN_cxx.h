// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#include "QTHN.h"

template <typename U> QTHN<U>::QTHN(const QTHN &qthn): TNamed(qthn), fNDims(qthn.fNDims), fAxes(NULL), fEntries(qthn.fEntries), fBinContent(NULL), fNBins(qthn.fNBins)
{
  Int_t i;
  fAxes=new TAxis*[fNDims];

  for(i=0; i<fNDims; i++) fAxes[i]=qthn.fAxes[i]?(TAxis*)qthn.fAxes[i]->Clone():NULL;
  fBinContent=(U*)malloc(fNBins*sizeof(U));
  memcpy(fBinContent,qthn.fBinContent,fNBins*sizeof(U));
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, Int_t ndims): TNamed(name,title), fNDims(ndims), fAxes(new TAxis*[ndims]), fBinContent(NULL), fNBins(0)
{
  if(fNDims<=0) {
    fprintf(stderr,"QTHN::QTHN: Error: Number of dimensions is invalid\n");
    throw 1;
  }
  memset(fAxes,0,fNDims*sizeof(TAxis*));
}

template <typename U> void QTHN<U>::AddBinContent(const Long64_t &bin, const U &w)
{
  Long64_t li;
  Long64_t bidx;

  if(bin<0 || bin>=fNBins) {
    fprintf(stderr,"QTHN::AddBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  fBinContent[bin]+=w;
  fEntries++;
}

template <typename U> void QTHN<U>::AddBinContent(const Int_t *coords, const U &w)
{
  AddBinContent(GetBin(coords),w);
}

template <typename U> void QTHN<U>::Clear(Option_t* option)
{
  TNamed::Clear(option);
  Int_t i;

  for(i=0; i<fNDims; i++) {
    if(fAxes[i]) delete fAxes[i];
  }
  delete[] fAxes;

  if(fNBins) {
    free(fBinContent);
  }
  fNDims=0;
  fAxes=NULL;
  fEntries=0;
  fBinContent=NULL;
  fNBins=0;
}

template <typename U> void QTHN<U>::ComputeNBins()
{
  Int_t i=0;
  fNBins=1;
  for(i=0; i<fNDims; i++) if(fAxes[i]) fNBins*=fAxes[i]->GetNbins()+2;

  if(fBinContent) free(fBinContent);
  fBinContent=(U*)malloc(fNBins*sizeof(U));
}

template <typename U> Int_t QTHN<U>::Fill(const Double_t *x, const U &w)
{
  Long64_t bin=FindBin(x);
  AddBinContent(bin,w);
  return bin;
}

template <typename U> Long64_t QTHN<U>::FindBin(const Double_t *x) const
{
  Int_t i;
  Long64_t bin=fAxes[fNDims-1]->FindFixBin(x[fNDims-1]);

  for(i=fNDims-2; i>=0; i--) {
    bin=bin*(fAxes[i]->GetNbins()+2)+fAxes[i]->FindFixBin(x[i]);
  }
  return bin;
}

template <typename U> TH1* QTHN<U>::GenTH(const char *name) const
{
  if(fNDims>3) {
    fprintf(stderr,"QTHN::GenTH: Error: Cannot generate a ROOT histogram because QTHN dimension is too high\n");
    throw 1;
  }

  TH1* th;
  Long64_t li;
  Int_t coords[3];
  Long64_t nfbins=GetNFbins();

  switch(fNDims) {
    case 1:
      if(!fAxes[0]->GetXbins()->fN) th=new TH1D(name,name,fAxes[0]->GetNbins(),fAxes[0]->GetXmin(),fAxes[0]->GetXmax());
      else th=new TH1D(name,name,fAxes[0]->GetNbins(),fAxes[0]->GetXbins()->GetArray());

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,coords);
	th->SetBinContent(coords[0],fBinContent[li]);
      }
      break;

    case 2:
      th=new TH2D(name,name,fAxes[0]->GetNbins(),fAxes[0]->GetXmin(),fAxes[0]->GetXmax(),fAxes[1]->GetNbins(),fAxes[1]->GetXmin(),fAxes[1]->GetXmax());
      if(fAxes[0]->GetXbins()->fN) th->GetXaxis()->Set(fAxes[0]->GetNbins(),fAxes[0]->GetXbins()->GetArray());
      if(fAxes[1]->GetXbins()->fN) th->GetYaxis()->Set(fAxes[1]->GetNbins(),fAxes[1]->GetXbins()->GetArray());

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,coords);
	th->SetBinContent(coords[0],coords[1],fBinContent[li]);
      }
      break;

    case 3:
      th=new TH3D(name,name,fAxes[0]->GetNbins(),fAxes[0]->GetXmin(),fAxes[0]->GetXmax(),fAxes[1]->GetNbins(),fAxes[1]->GetXmin(),fAxes[1]->GetXmax(),fAxes[2]->GetNbins(),fAxes[2]->GetXmin(),fAxes[2]->GetXmax());
      if(fAxes[0]->GetXbins()->fN) th->GetXaxis()->Set(fAxes[0]->GetNbins(),fAxes[0]->GetXbins()->GetArray());
      if(fAxes[1]->GetXbins()->fN) th->GetYaxis()->Set(fAxes[1]->GetNbins(),fAxes[1]->GetXbins()->GetArray());
      if(fAxes[2]->GetXbins()->fN) th->GetZaxis()->Set(fAxes[2]->GetNbins(),fAxes[2]->GetXbins()->GetArray());

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,coords);
	th->SetBinContent(coords[0],coords[1],coords[2],fBinContent[li]);
      }
      break;

    default:
      return NULL;
  }

  th->SetEntries(fEntries);
  return th;
}

template <typename U> TAxis* QTHN<U>::GetAxis(Int_t axis) const
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::GetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  return fAxes[axis];
}

template <typename U> Long64_t QTHN<U>::GetBin(const Int_t *coords) const
{
  Int_t i;
  Long64_t bin=coords[fNDims-1];

  for(i=fNDims-2; i>=0; i--) {
    bin=bin*(fAxes[i]->GetNbins()+2)+coords[i];
  }
  return bin;
}

template <typename U> const U& QTHN<U>::GetBinContent(const Int_t *coords) const
{
  Long64_t bin=GetBin(coords);
  return GetBinContent(bin);
}

template <typename U> void QTHN<U>::GetBinCoords(Long64_t bin, Int_t *coords) const
{
  Int_t i;
  coords[0]=bin%(fAxes[0]->GetNbins()+2);

  for(i=1; i<fNDims; i++) {
    bin=(bin-coords[i-1])/(fAxes[i-1]->GetNbins()+2);
    coords[i]=bin%(fAxes[i]->GetNbins()+2);
  }
}

template <typename U> void QTHN<U>::GetCorrelationMatrix(TMatrixDSym* covmat, Bool_t width, Bool_t varianceondiag) const
{
  Int_t i,j;

  GetCovarianceMatrix(covmat,width);

  for(i=0; i<fNDims; i++) {

    for(j=i+1; j<fNDims; j++) {
      (*covmat)[i][j]=(*covmat)[j][i]=(*covmat)[i][j]/TMath::Sqrt((*covmat)[i][i]*(*covmat)[j][j]);
    }
  }

  if(!varianceondiag) {
    for(i=0; i<fNDims; i++) (*covmat)[i][i]=1;
  }
}

template <typename U> void QTHN<U>::GetCovarianceMatrix(TMatrixDSym* covmat, Bool_t width) const
{
  Int_t i,j;
  Long64_t li;
  TAxis **vbsaxis=new TAxis*[fNDims];      //array of axis using variable bin width
  Int_t *vbaindex=new Int_t[fNDims];
  Int_t nvbaxes=0;
  Int_t *coords=new Int_t[fNDims];
  Double_t *means=new Double_t[fNDims];
  Double_t *covs=new Double_t[fNDims*(fNDims+1)/2];
  Double_t integral=0;
  memset(means,0,fNDims*sizeof(Double_t));
  memset(covs,0,fNDims*(fNDims+1)/2*sizeof(Double_t));
  Long64_t nfbins=GetNFbins();

  if(width) {
    memset(vbsaxis,0,fNDims*sizeof(TAxis*)); //Initialize axis pointers to NULL

    for(i=0; i<fNDims; i++) {
      //Add TAxis pointers to vbsaxis for axis having variable bin width

      if(fAxes[i]->GetNbins()>1 && fAxes[i]->GetXbins()->fN) {
	vbsaxis[nvbaxes]=fAxes[i];
	vbaindex[nvbaxes]=i;
	nvbaxes++;
      }
    }
  }

  if(width && nvbaxes) {
    Double_t binvol;

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      binvol=vbsaxis[0]->GetBinWidth(coords[vbaindex[0]]);

      for(i=1; i<nvbaxes; i++) binvol*=vbsaxis[i]->GetBinWidth(coords[vbaindex[i]]);
      binvol*=fBinContent[li];
      integral+=binvol;

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*binvol;

	for(j=i; j<fNDims; j++) {
	  covs[(2*fNDims-1-i)*i/2+j]+=fAxes[i]->GetBinCenter(coords[i])*fAxes[j]->GetBinCenter(coords[j])*binvol;
	}
      }
    }

  } else {

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      integral+=fBinContent[li];

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*fBinContent[li];

	for(j=i; j<fNDims; j++) {
	  covs[(2*fNDims-1-i)*i/2+j]+=fAxes[i]->GetBinCenter(coords[i])*fAxes[j]->GetBinCenter(coords[j])*fBinContent[li];
	}
      }
    }
  }

  covmat->ResizeTo(fNDims,fNDims);
  memset(covmat->GetMatrixArray(),0,fNDims*fNDims*sizeof(Double_t));

  if(integral) {

    for(i=0; i<fNDims; i++) {
      means[i]/=integral;
    }

    for(i=0; i<fNDims; i++) {

      for(j=i; j<fNDims; j++) {
	(*covmat)[i][j]=(*covmat)[j][i]=covs[(2*fNDims-1-i)*i/2+j]/integral-means[i]*means[j];
      }
    }
  }

  delete[] means;
  delete[] covs;
  delete[] coords;
  delete[] vbsaxis;
  delete[] vbaindex;
}

template <typename U> void QTHN<U>::GetMeans(Double_t means[], Bool_t width) const
{
  Int_t i;
  Long64_t li;
  TAxis **vbsaxis=new TAxis*[fNDims];      //array of axis using variable bin width
  Int_t *vbaindex=new Int_t[fNDims];
  Int_t nvbaxes=0;
  Int_t *coords=new Int_t[fNDims];
  Double_t integral=0;
  memset(means,0,fNDims*sizeof(Double_t));
  Long64_t nfbins=GetNFbins();

  if(width) {
    memset(vbsaxis,0,fNDims*sizeof(TAxis*)); //Initialize axis pointers to NULL

    for(i=0; i<fNDims; i++) {
      //Add TAxis pointers to vbsaxis for axis having variable bin width

      if(fAxes[i]->GetNbins()>1 && fAxes[i]->GetXbins()->fN) {
	vbsaxis[nvbaxes]=fAxes[i];
	vbaindex[nvbaxes]=i;
	nvbaxes++;
      }
    }
  }

  if(width && nvbaxes) {
    Double_t binvol;

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      binvol=vbsaxis[0]->GetBinWidth(coords[vbaindex[0]]);

      for(i=1; i<nvbaxes; i++) binvol*=vbsaxis[i]->GetBinWidth(coords[vbaindex[i]]);
      binvol*=fBinContent[li];
      integral+=binvol;

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*binvol;
      }
    }

  } else {

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      integral+=fBinContent[li];

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*fBinContent[li];
      }
    }
  }

  if(integral) {

    for(i=0; i<fNDims; i++) {
      means[i]/=integral;
    }
  }

  delete[] coords;
  delete[] vbsaxis;
  delete[] vbaindex;
}

template <typename U> Double_t QTHN<U>::Integral(Int_t** binranges, Bool_t *widths) const
{
  Int_t mins[fNDims], maxs[fNDims];
  Int_t i, j[fNDims];
  Long64_t li;
  Bool_t cbw=kTRUE; //Constant bin width in all directions
  Bool_t bwi=kFALSE;//Integration using bin width for at least some direction
  Long64_t nfbins=GetNFbins();

  for(i=0;i<fNDims;i++){

    if(binranges && binranges[i]){
      mins[i]=(*(binranges[i])>1)?*(binranges[i]):1;
      maxs[i]=(*(binranges[i]+1)<fAxes[i]->GetNbins())?*(binranges[i]+1):fAxes[i]->GetNbins(); 
    } else {
      mins[i]=1;
      maxs[i]=fAxes[i]->GetNbins();
    }
    if(fAxes[i]->GetXbins()->fN) cbw=kFALSE;
    if(!widths || widths[i]) bwi=kTRUE;
    //cout << mins[i] << "\t" << maxs[i] << "\n";
  }

  Double_t integral=0;
  Double_t binvol;

  //If bin width is constant in all directions
  if(cbw) {

    //If integrating using bin width for all axes
    if(!widths) {
      binvol=fAxes[0]->GetBinWidth(1);
      for(i=1; i<fNDims; i++) binvol*=fAxes[i]->GetBinWidth(1);

      //Else if using bin width for only some axes
    } else {
      binvol=1;
      for(i=0; i<fNDims; i++) if(widths[i]) binvol*=fAxes[i]->GetBinWidth(1);
    }

    for(li=0; li<nfbins; li++) {
      if(IsFBinIncluded(li,mins,maxs)) integral+=fBinContent[li];
    }
    integral*=binvol;

  //Else if bin width is not constant for some direction
  } else {

    //If integrating using bin width for all axes
    if(!widths) {

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,j);

	for(i=0; i<fNDims; i++) if(j[i]<mins[i] || j[i]>maxs[i]) break; 

	if(i==fNDims) {
	  binvol=fAxes[0]->GetBinWidth(j[0]);
	  for(i=1; i<fNDims; i++) binvol*=fAxes[i]->GetBinWidth(j[i]);
	  integral+=fBinContent[li]*binvol;
	}
      }

      //Else if integrating without using bin width at all
    } else if(!bwi) {

      for(li=0; li<nfbins; li++) {
	if(IsFBinIncluded(li,mins,maxs)) integral+=fBinContent[li];
      }

      //Else if integrating using bin width for some of the directions only
    } else {

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,j);

	for(i=0; i<fNDims; i++) if(j[i]<mins[i] || j[i]>maxs[i]) break; 

	if(i==fNDims) {
	  binvol=(widths[0]?fAxes[0]->GetBinWidth(j[0]):1);
	  for(i=1; i<fNDims; i++) binvol*=(widths[i]?fAxes[i]->GetBinWidth(j[i]):1);
	  integral+=fBinContent[li]*binvol;
	}
      }
    }
  }
  return integral;
}

template <typename U> Bool_t QTHN<U>::IsConstantBW(const Int_t &nbins, const Double_t *bins) const
{
  if(!nbins) return kTRUE;
  Double_t bw=bins[1]-bins[0];

  for(Int_t i=1; i<nbins; i++) {
    if(bins[i+1]-bins[i]!=bw) return kFALSE;
  }
  return kTRUE;
}

template <typename U> Bool_t QTHN<U>::IsFBinIncluded(const Long64_t &fbin, const Int_t *mins, const Int_t *maxs) const
{
  Int_t i;
  Long64_t bin=fbin;
  Int_t coord=bin%(fAxes[0]->GetNbins()+2);
  if(coord<mins[0] || coord>maxs[0]) return kFALSE;

  for(i=1; i<fNDims; i++) {
    bin=(bin-coord)/(fAxes[i-1]->GetNbins()+2);
    coord=bin%(fAxes[i]->GetNbins()+2);
    if(coord<mins[i] || coord>maxs[i]) return kFALSE;
  }
  return kTRUE;
}

template <typename U> const QTHN<U>& QTHN<U>::operator=(const QTHN<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
  fNDims=qthn.fNDims;
  fEntries=qthn.fEntries;
  fNBins=qthn.fNBins;
  Int_t i;
  fAxes=new TAxis*[fNDims];

  for(i=0; i<fNDims; i++) fAxes[i]=qthn.fAxes[i]?(TAxis*)qthn.fAxes[i]->Clone():NULL;
  fBinContent=(U*)malloc(fNBins*sizeof(U));
  memcpy(fBinContent,qthn.fBinContent,fNBins*sizeof(U));
  return *this;
}

template <typename U> void QTHN<U>::Reset()
{
  memset(fBinContent,0,fNBins*sizeof(Double_t));
  fEntries=0;
}

template <typename U> QTHN<U>* QTHN<U>::Projection(const char *name, const Int_t *axes, Int_t naxes, QTHN<U> *th) const
{
  const Int_t nsdims=fNDims-naxes;

  if(naxes<=0 || !axes || naxes>=fNDims) return NULL;

  Int_t i,j,l,m;

  for(i=0; i<naxes; i++) {
    if(axes[i]<0 || axes[i]>=fNDims) {
      fprintf(stderr,"QTHN<U>::Projection: Error: Invalid axis index: %i\n",axes[i]);
      throw 1;
    }
  }

  if(th) {
    th->SetNDims(naxes);
    th->SetNameTitle(name,name);

  } else th=new QTHN<U>(name,name,naxes);

  for(i=0; i<naxes; i++) {
    th->SetAxis(i,fAxes[axes[i]]);
  }

  Int_t *indices=new Int_t[nsdims]; //Axes indices for axes that are not projected
  Int_t *biniter=new Int_t[fNDims]; //Integers used as indices for iteration over bins of original histogram
  Int_t *pbiniter=new Int_t[naxes]; //Integers used as indices for iteration over bins of projected histogram
  U dbuf;
  l=0;

  for(i=0; i<fNDims; i++) {

    m=0;
    for(j=0; j<naxes; j++) if(axes[j]==i) m++;
    if(!m) {
      indices[l]=i;
      l++;
    }
  }

  for(i=0; i<naxes; i++) biniter[axes[i]]=1;

  //Loop over bin indices of projection axes
  do{
    dbuf=0;

    for(i=0; i<nsdims; i++) biniter[indices[i]]=1;

    do{
      dbuf+=GetBinContent(GetBin(biniter));
      biniter[indices[0]]++;
      
      i=0;
      while(biniter[indices[i]]>fAxes[indices[i]]->GetNbins()){
	biniter[indices[i]]=1;
	i++;

	if(i>=nsdims) break;
	biniter[indices[i]]++;
      }

    } while(i<nsdims);

    for(i=0; i<naxes; i++) pbiniter[i]=biniter[axes[i]];
    th->SetBinContent(pbiniter,dbuf);

    biniter[axes[0]]++;

    i=0;
    while(biniter[axes[i]]>fAxes[axes[i]]->GetNbins()){
      biniter[axes[i]]=1;
      i++;

      if(i>=naxes) break;
      biniter[axes[i]]++;
    }

  } while(i<naxes);

  delete[] indices;
  delete[] biniter;
  delete[] pbiniter;

  th->fEntries=fEntries;
  return th;
}

template <typename U> void QTHN<U>::Scale(const Double_t &scale)
{
  Long64_t li;
  Long64_t nfbins=GetNFbins();

  for(li=0; li<nfbins; li++) fBinContent[li]*=(U)scale;
}

template <typename U> void QTHN<U>::ScaleBinContent(const Long64_t &bin, const Double_t &scale)
{
  Long64_t fbin=GetFBin(bin);
  if(fbin!=-1) fBinContent[fbin]*=(U)scale;
}

template <typename U> void QTHN<U>::ScaleBinContent(const Int_t *coords, const Double_t &scale)
{
  Long64_t fbin=GetFBin(coords);
  if(fbin!=-1) fBinContent[fbin]*=(U)scale;
}

template <typename U> void QTHN<U>::ScaleFBinContent(const Long64_t &fbin, const Double_t &scale)
{
  if(fbin<0 || fbin>=GetNFbins()) {
    fprintf(stderr,"QTHN::ScaleFBinContent: Error: Invalid bin index\n");
    throw 1;
  }
  fBinContent[fbin]*=(U)scale;
}


template <typename U> void QTHN<U>::SetAxis(Int_t axis, Int_t nbins, Double_t min, Double_t max)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new TAxis(nbins,min,max);
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::SetAxis(Int_t axis, Int_t nbins, Double_t *bins)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }

  if(fAxes[axis]) delete fAxes[axis];

  if(IsConstantBW(nbins,bins)) fAxes[axis]=new TAxis(nbins,bins[0],bins[nbins]);
  else fAxes[axis]=new TAxis(nbins,bins);
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::SetAxis(Int_t axis, const TAxis *anaxis)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=(TAxis*)anaxis->Clone();
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::SetBinContent(const Long64_t &bin, const U &content)
{
  if(bin<0 || bin>=fNBins) {
    fprintf(stderr,"QTHN::SetBinContent: Error: Invalid bin index\n");
    throw 1;
  }
  fBinContent[bin]=content;
}

template <typename U> void QTHN<U>::SetBinContent(const Int_t *coords, const U &content)
{
  SetBinContent(GetBin(coords),content);
}

#include "QTHN_Dict_cxx.h"
