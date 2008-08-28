#include "QTHN.h"

template <typename U> QTHN<U>::QTHN(const QTHN &qthn): TNamed(qthn), fNDims(qthn.fNDims), fAxes(NULL), fNBins(qthn.fNBins), fBins(NULL), fEntries(qthn.fEntries), fBinContent(NULL), fZero(qthn.fZero), fMaxNBins(qthn.fMaxNBins)
{
  Int_t i;
  fAxes=new TAxis*[fNDims];

  for(i=0; i<fNDims; i++) fAxes[i]=(TAxis*)qthn.fAxes[i]->Clone();
  fBins=(Long64_t*)malloc(fNBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNBins*sizeof(Long64_t));
  fBinContent=(U*)malloc(fNBins*sizeof(U));
  memcpy(fBinContent,qthn.fBinContent,fNBins*sizeof(U));
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, Int_t ndims): TNamed(name,title), fNDims(ndims), fAxes(new TAxis*[ndims]), fNBins(0), fBins(NULL), fBinContent(NULL), fZero(0), fMaxNBins(0)
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

  if(bin<0 || bin>=fMaxNBins) {
    fprintf(stderr,"QTHN::AddBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  bidx=std::lower_bound(fBins,fBins+fNBins, bin)-fBins;

  if(bidx==fNBins || fBins[bidx]!=bin) {

    if(w) {
      fNBins++;
      fBins=(Long64_t*)realloc(fBins,fNBins*sizeof(Long64_t));
      fBinContent=(U*)realloc(fBinContent,fNBins*sizeof(U));

      if(!fBins || !fBinContent) {
	fprintf(stderr,"QTHN::AddBinContent: Error: Could not allocate memory\n");
	throw 1;
      }

      for(li=fNBins-1; li>bidx; li--) {
	fBins[li]=fBins[li-1];
	fBinContent[li]=fBinContent[li-1];
      }
      fBins[bidx]=bin;
      fBinContent[bidx]=w;
    }

  } else {
    fBinContent[bidx]+=w;
  }
  fEntries++;
}

template <typename U> void QTHN<U>::AddBinContent(const Int_t *coords, const U &w)
{
  AddBinContent(GetBin(coords),w);
}

template <typename U> void QTHN<U>::AddFBinContent(const Long64_t &fbin, const U &w)
{
  if(fbin<0 || fbin>=fNBins) {
    fprintf(stderr,"QTHN::AddFBinContent: Error: Invalid bin index\n");
    throw 1;
  }
  fBinContent[fbin]+=w;
  fEntries++;
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
    free(fBins);
    free(fBinContent);
  }
  fNDims=0;
  fAxes=NULL;
  fNBins=0;
  fBins=NULL;
  fEntries=0;
  fBinContent=0;
  fMaxNBins=0;
}

template <typename U> void QTHN<U>::ComputeMaxNBins()
{
  Int_t i=0;
  fMaxNBins=fAxes[0]->GetNbins()+2;
  for(i=1; i<fNDims; i++) fMaxNBins*=fAxes[i]->GetNbins()+2;
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

template <typename U> Long64_t QTHN<U>::GetFBin(const Int_t *coords) const
{
  Long64_t bin=GetBin(coords);
  Long64_t bidx;

  bidx=std::lower_bound(fBins,fBins+fNBins, bin)-fBins;

  if(bidx==fNBins || fBins[bidx]!=bin) return -1;
  return bidx;
}

template <typename U> Long64_t QTHN<U>::GetFBin(const Long64_t &bin) const
{
  Long64_t bidx=std::lower_bound(fBins,fBins+fNBins, bin)-fBins;

  if(bidx==fNBins || fBins[bidx]!=bin) return -1;
  return bidx;
}

template <typename U> const U& QTHN<U>::GetBinContent(const Long64_t &bin) const
{
  Long64_t li=TMath::BinarySearch(fNBins, fBins, bin);
  if(fBins[li]==bin) return fBinContent[li];
  return fZero;
}

template <typename U> const U& QTHN<U>::GetFBinContent(const Long64_t &fbin) const
{
  if(fbin<0 || fbin>=fNBins) {
    fprintf(stderr,"QTHN::GetFBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  return fBinContent[fbin];
}

template <typename U> void QTHN<U>::GetBinCoords(Long64_t &bin, Int_t *coords) const
{
  Int_t i;
  coords[0]=bin%(fAxes[0]->GetNbins()+2);

  for(i=1; i<fNDims; i++) {
    bin=(bin-coords[i-1])/(fAxes[i-1]->GetNbins()+2);
    coords[i]=bin%(fAxes[i]->GetNbins()+2);
  }
}

template <typename U> const Long64_t& QTHN<U>::GetFBinCoord(const Long64_t &fbin) const
{
  if(fbin<0 || fbin>=fNBins) {
    fprintf(stderr,"QTHN::GetFBinCoord: Error: Invalid bin index\n");
    throw 1;
  }
  return fBins[fbin];
}

template <typename U> void QTHN<U>::GetFBinCoords(const Long64_t &fbin, Int_t *coords) const
{
  Int_t i;
  Long64_t bin=fBins[fbin];
  coords[0]=bin%(fAxes[0]->GetNbins()+2);

  for(i=1; i<fNDims; i++) {
    bin=(bin-coords[i-1])/(fAxes[i-1]->GetNbins()+2);
    coords[i]=bin%(fAxes[i]->GetNbins()+2);
  }
}

template <typename U> Bool_t QTHN<U>::IsBinIncluded(Long64_t &bin, const Int_t *mins, const Int_t *maxs) const
{
  Int_t i;
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
  fNBins=qthn.fNBins;
  fEntries=qthn.fEntries;
  Int_t i;
  fAxes=new TAxis*[fNDims];

  for(i=0; i<fNDims; i++) fAxes[i]=(TAxis*)qthn.fAxes[i]->Clone();
  fBins=(Long64_t*)malloc(fNBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNBins*sizeof(Long64_t));
  fBinContent=(U*)malloc(fNBins*sizeof(U));
  memcpy(fBinContent,qthn.fBinContent,fNBins*sizeof(U));
  return *this;
}

template <typename U> void QTHN<U>::Reset()
{
  if(fNBins) {
    free(fBins); fBins=NULL;
    free(fBinContent); fBinContent=NULL;
    fNBins=0;
  }
  fEntries=0;
}

template <typename U> TH1D* QTHN<U>::Projection1D(const char *name, Int_t xaxis) const
{
  if(xaxis<0 || xaxis>=fNDims) {
    fprintf(stderr,"QTHN::Projection1D: Error: Invalid axis index\n");
    throw 1;
  }
  TH1D *th;

  if(!fAxes[xaxis]->GetXbins()->fN) {
    th=new TH1D(name,name,fAxes[xaxis]->GetNbins(),fAxes[xaxis]->GetXmin(),fAxes[xaxis]->GetXmax());

  } else {
    th=new TH1D(name,name,fAxes[xaxis]->GetNbins(),fAxes[xaxis]->GetXbins()->GetArray());
  }

  Int_t *indices=new Int_t[fNDims-1];
  Int_t *is=new Int_t[fNDims-1];
  Int_t *coords=new Int_t[fNDims];
  Int_t i,l,m;
  Double_t dbuf;
  l=0;

  for(i=0; i<fNDims; i++) {

    if(i!=xaxis) {
      indices[l]=i;
      l++;
    }
  }

  for(i=1; i<=fAxes[xaxis]->GetNbins(); i++) {
    dbuf=0.;

    for(l=0; l<fNDims-1; l++) is[l]=1;
    l=fNDims-2;

    while(is[0]<=fAxes[indices[0]]->GetNbins()) {

      for(m=0; m<fNDims-1; m++) coords[indices[m]]=is[m];
      coords[xaxis]=i;
      dbuf+=GetBinContent(GetBin(coords));
      is[l]++;

      while(l>0 && is[l]>fAxes[indices[l]]->GetNbins()) {
	is[l-1]++;
	is[l]=1;
	l--;
      }
      l=fNDims-2;
    }
    th->SetBinContent(i,dbuf);
  }

  delete[] indices;
  delete[] is;
  delete[] coords;

  return th;
}

template <typename U> TH2D* QTHN<U>::Projection2D(const char *name, Int_t xaxis, Int_t yaxis) const
{
  if(xaxis<0 || xaxis>=fNDims) {
    fprintf(stderr,"QTHN::Projection1D: Error: Invalid xaxis index\n");
    throw 1;
  }
  if(yaxis<0 || yaxis>=fNDims) {
    fprintf(stderr,"QTHN::Projection1D: Error: Invalid yaxis index\n");
    throw 1;
  }
  TH2D *th=new TH2D(name,name,fAxes[xaxis]->GetNbins(),fAxes[xaxis]->GetXmin(),fAxes[xaxis]->GetXmax(),fAxes[yaxis]->GetNbins(),fAxes[yaxis]->GetXmin(),fAxes[yaxis]->GetXmax());
  if(fAxes[xaxis]->GetXbins()->fN) th->GetXaxis()->Set(fAxes[xaxis]->GetNbins(),fAxes[xaxis]->GetXbins()->GetArray());
  if(fAxes[yaxis]->GetXbins()->fN) th->GetYaxis()->Set(fAxes[yaxis]->GetNbins(),fAxes[yaxis]->GetXbins()->GetArray());

  Int_t *indices=new Int_t[fNDims];
  Int_t *is=new Int_t[fNDims];
  Int_t *coords=new Int_t[fNDims];
  Int_t i,j,l,m;
  Int_t ndiff=0;
  Double_t dbuf;
  l=0;

  for(i=0; i<fNDims; i++) {

    if(i!=xaxis && i!=yaxis) {
      indices[l]=i;
      l++;
    }
  }
  ndiff=fNDims-l;

  if(fNDims-ndiff==0) return th;

  for(i=1; i<=fAxes[xaxis]->GetNbins(); i++) {

    for(j=1; j<=fAxes[yaxis]->GetNbins(); j++) {
      dbuf=0.;

      for(l=0; l<fNDims-ndiff; l++) is[l]=1;
      l=fNDims-ndiff-1;

      while(is[0]<=fAxes[indices[0]]->GetNbins()) {

	for(m=0; m<fNDims-ndiff; m++) coords[indices[m]]=is[m];
	coords[xaxis]=i;
	coords[yaxis]=j;
	dbuf+=GetBinContent(GetBin(coords));
	is[l]++;

	while(l>0 && is[l]>fAxes[indices[l]]->GetNbins()) {
	  is[l-1]++;
	  is[l]=1;
	  l--;
	}
	l=fNDims-ndiff-1;
      }
      th->SetBinContent(i,j,dbuf);
    }
  }

  delete[] indices;
  delete[] is;
  delete[] coords;

  return th;
}

template <typename U> TH3D* QTHN<U>::Projection3D(const char *name, Int_t xaxis, Int_t yaxis, Int_t zaxis) const
{
  if(xaxis<0 || xaxis>=fNDims) {
    fprintf(stderr,"QTHN::Projection1D: Error: Invalid xaxis index\n");
    throw 1;
  }
  if(yaxis<0 || yaxis>=fNDims) {
    fprintf(stderr,"QTHN::Projection1D: Error: Invalid yaxis index\n");
    throw 1;
  }
  if(zaxis<0 || zaxis>=fNDims) {
    fprintf(stderr,"QTHN::Projection1D: Error: Invalid zaxis index\n");
    throw 1;
  }
  TH3D *th=new TH3D(name,name,fAxes[xaxis]->GetNbins(),fAxes[xaxis]->GetXmin(),fAxes[xaxis]->GetXmax(),fAxes[yaxis]->GetNbins(),fAxes[yaxis]->GetXmin(),fAxes[yaxis]->GetXmax(),fAxes[zaxis]->GetNbins(),fAxes[zaxis]->GetXmin(),fAxes[zaxis]->GetXmax());
  if(fAxes[xaxis]->GetXbins()->fN) th->GetXaxis()->Set(fAxes[xaxis]->GetNbins(),fAxes[xaxis]->GetXbins()->GetArray());
  if(fAxes[yaxis]->GetXbins()->fN) th->GetYaxis()->Set(fAxes[yaxis]->GetNbins(),fAxes[yaxis]->GetXbins()->GetArray());
  if(fAxes[zaxis]->GetXbins()->fN) th->GetZaxis()->Set(fAxes[zaxis]->GetNbins(),fAxes[zaxis]->GetXbins()->GetArray());

  Int_t *indices=new Int_t[fNDims];
  Int_t *is=new Int_t[fNDims];
  Int_t *coords=new Int_t[fNDims];
  Int_t i,j,k,l,m;
  Int_t ndiff=0;
  Double_t dbuf;
  l=0;

  for(i=0; i<fNDims; i++) {

    if(i!=xaxis && i!=yaxis && i!=zaxis) {
      indices[l]=i;
      l++;
    }
  }
  ndiff=fNDims-l;

  if(fNDims-ndiff==0) return th;

  for(i=1; i<=fAxes[xaxis]->GetNbins(); i++) {

    for(j=1; j<=fAxes[yaxis]->GetNbins(); j++) {

      for(k=1; k<=fAxes[zaxis]->GetNbins(); k++) {
	dbuf=0.;

	for(l=0; l<fNDims-ndiff; l++) is[l]=1;
	l=fNDims-ndiff-1;

	while(is[0]<=fAxes[indices[0]]->GetNbins()) {

	  for(m=0; m<fNDims-ndiff; m++) coords[indices[m]]=is[m];
	  coords[xaxis]=i;
	  coords[yaxis]=j;
	  coords[zaxis]=k;
	  dbuf+=GetBinContent(GetBin(coords));
	  is[l]++;

	  while(l>0 && is[l]>fAxes[indices[l]]->GetNbins()) {
	    is[l-1]++;
	    is[l]=1;
	    l--;
	  }
	  l=fNDims-ndiff-1;
	}
	th->SetBinContent(i,j,k,dbuf);
      }
    }
  }

  delete[] indices;
  delete[] is;
  delete[] coords;

  return th;
}

template <typename U> void QTHN<U>::Scale(const Double_t &scale)
{
  Long64_t li;

  for(li=0; li<fNBins; li++) fBinContent[li]*=scale;
}

template <typename U> void QTHN<U>::ScaleBinContent(const Long64_t &bin, const Double_t &scale)
{
  Long64_t fbin=GetFBin(bin);
  if(fbin!=-1) fBinContent[fbin]*=scale;
}

template <typename U> void QTHN<U>::ScaleBinContent(const Int_t *coords, const Double_t &scale)
{
  Long64_t fbin=GetFBin(coords);
  if(fbin!=-1) fBinContent[fbin]*=scale;
}

template <typename U> void QTHN<U>::ScaleFBinContent(const Long64_t &fbin, const Double_t &scale)
{
  if(fbin<0 || fbin>=fNBins) {
    fprintf(stderr,"QTHN::SetFBinContent: Error: Invalid bin index\n");
    throw 1;
  }
  fBinContent[fbin]*=scale;
}


template <typename U> void QTHN<U>::SetAxis(Int_t axis, Int_t nbins, Double_t min, Double_t max)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new TAxis(nbins,min,max);
  ComputeMaxNBins();
}

template <typename U> void QTHN<U>::SetAxis(Int_t axis, Int_t nbins, Double_t *bins)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new TAxis(nbins,bins);
  ComputeMaxNBins();
}

template <typename U> void QTHN<U>::SetBinContent(const Long64_t &bin, const Double_t &content)
{
  Long64_t li;
  Long64_t bidx;

  if(bin<0 || bin>=fMaxNBins) {
    fprintf(stderr,"QTHN::SetBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  bidx=std::lower_bound(fBins,fBins+fNBins, bin)-fBins;

  if(bidx==fNBins || fBins[bidx]!=bin) {

    if(content) {
      fNBins++;
      fBins=(Long64_t*)realloc(fBins,fNBins*sizeof(Long64_t));
      fBinContent=(U*)realloc(fBinContent,fNBins*sizeof(U));

      if(!fBins || !fBinContent) {
	fprintf(stderr,"QTHN::AddBinContent: Error: Could not allocate memory\n");
	throw 1;
      }

      for(li=fNBins-1; li>bidx; li--) {
	fBins[li]=fBins[li-1];
	fBinContent[li]=fBinContent[li-1];
      }
      fBins[bidx]=bin;
      fBinContent[bidx]=content;
    }

  } else {
    fBinContent[bidx]=content;
  }
}

template <typename U> void QTHN<U>::SetBinContent(const Int_t *coords, const Double_t &content)
{
  SetBinContent(GetBin(coords),content);
}

template <typename U> void QTHN<U>::SetFBinContent(const Long64_t &fbin, const Double_t &content)
{
  if(fbin<0 || fbin>=fNBins) {
    fprintf(stderr,"QTHN::SetFBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  if(content) {
    fBinContent[fbin]=content;

  } else {
    fNBins--;

    for(Long64_t li=fbin; li<fNBins; li++) {
      fBins[li]=fBins[li+1];
      fBinContent[li]=fBinContent[li+1];
    }
    fBins=(Long64_t*)realloc(fBins,fNBins*sizeof(Long64_t));
    fBinContent=(U*)realloc(fBinContent,fNBins*sizeof(U));
  }
}

#include "QTHN_Dict_cxx.h"
