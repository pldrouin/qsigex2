// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#include "QTHNF.h"

template <typename U> QTHNF<U>::QTHNF(const QTHNF &qthn): QTHN<U>(), fNFBins(qthn.fNFBins), fBins(NULL), fZero(qthn.fZero)
{
  SetNameTitle(qthn.GetName(),qthn.GetTitle());
  Int_t i;
  QTHN<U>::fNDims=qthn.fNDims;
  QTHN<U>::fAxes=new TAxis*[QTHN<U>::fNDims];
  for(i=0; i<QTHN<U>::fNDims; i++) QTHN<U>::fAxes[i]=qthn.fAxes[i]?(TAxis*)qthn.fAxes[i]->Clone():NULL;
  QTHN<U>::fEntries=qthn.fEntries;
  QTHN<U>::fNBins=qthn.fNBins;
  fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNFBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));
  memcpy(QTHN<U>::fBinContent,qthn.fBinContent,fNFBins*sizeof(U));
}

template <typename U> void QTHNF<U>::AddBinContent(const Long64_t &bin, const U &w)
{
  Long64_t li;
  Long64_t bidx;

  if(bin<0 || bin>=QTHN<U>::fNBins) {
    fprintf(stderr,"QTHNF::AddBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) {

    if(w) {
      fNFBins++;
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));

      for(li=fNFBins-1; li>bidx; li--) {
	fBins[li]=fBins[li-1];
	QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li-1];
      }
      fBins[bidx]=bin;
      QTHN<U>::fBinContent[bidx]=w;
    }

  } else {
    QTHN<U>::fBinContent[bidx]+=w;
  }
  QTHN<U>::fEntries++;
}

template <typename U> void QTHNF<U>::AddBinContent(const Int_t *coords, const U &w)
{
  AddBinContent(GetBin(coords),w);
}

template <typename U> void QTHNF<U>::AddFBinContent(const Long64_t &fbin, const U &w)
{
  if(fbin<0 || fbin>=fNFBins) {
    fprintf(stderr,"QTHNF::AddFBinContent: Error: Invalid bin index\n");
    throw 1;
  }
  QTHN<U>::fBinContent[fbin]+=w;
  QTHN<U>::fEntries++;
}

template <typename U> void QTHNF<U>::Clear(Option_t* option)
{
  QTHN<U>::Clear(option);

  if(fNFBins) {
    free(fBins);
  }
  fNFBins=0;
  fBins=NULL;
}

template <typename U> void QTHNF<U>::ComputeNBins()
{
  Int_t i=0;
  QTHN<U>::fNBins=1;
  for(i=0; i<QTHN<U>::fNDims; i++) if(QTHN<U>::fAxes[i]) QTHN<U>::fNBins*=QTHN<U>::fAxes[i]->GetNbins()+2;
}

template <typename U> Long64_t QTHNF<U>::GetFBin(const Int_t *coords) const
{
  Long64_t bin=GetBin(coords);
  Long64_t bidx;

  bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) return -1;
  return bidx;
}

template <typename U> Long64_t QTHNF<U>::GetFBin(const Long64_t &bin) const
{
  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) return -1;
  return bidx;
}

template <typename U> const U& QTHNF<U>::GetBinContent(const Long64_t &bin) const
{
  Long64_t li=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(li==fNFBins || fBins[li]!=bin) return fZero;
  return QTHN<U>::fBinContent[li];
}

template <typename U> const U& QTHNF<U>::GetFBinContent(const Long64_t &fbin) const
{
  if(fbin<0 || fbin>=fNFBins) {
    fprintf(stderr,"QTHNF::GetFBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  return QTHN<U>::fBinContent[fbin];
}

template <typename U> const Long64_t& QTHNF<U>::GetFBinCoord(const Long64_t &fbin) const
{
  if(fbin<0 || fbin>=fNFBins) {
    fprintf(stderr,"QTHNF::GetFBinCoord: Error: Invalid bin index\n");
    throw 1;
  }
  return fBins[fbin];
}

template <typename U> void QTHNF<U>::GetFBinCoords(const Long64_t &fbin, Int_t *coords) const
{
  Int_t i;
  Long64_t bin=fBins[fbin];
  coords[0]=bin%(QTHN<U>::fAxes[0]->GetNbins()+2);

  for(i=1; i<QTHN<U>::fNDims; i++) {
    bin=(bin-coords[i-1])/(QTHN<U>::fAxes[i-1]->GetNbins()+2);
    coords[i]=bin%(QTHN<U>::fAxes[i]->GetNbins()+2);
  }
}

template <typename U> Bool_t QTHNF<U>::IsFBinIncluded(const Long64_t &fbin, const Int_t *mins, const Int_t *maxs) const
{
  Int_t i;
  Long64_t bin=fBins[fbin];
  Int_t coord=bin%(QTHN<U>::fAxes[0]->GetNbins()+2);
  if(coord<mins[0] || coord>maxs[0]) return kFALSE;

  for(i=1; i<QTHN<U>::fNDims; i++) {
    bin=(bin-coord)/(QTHN<U>::fAxes[i-1]->GetNbins()+2);
    coord=bin%(QTHN<U>::fAxes[i]->GetNbins()+2);
    if(coord<mins[i] || coord>maxs[i]) return kFALSE;
  }
  return kTRUE;
}

template <typename U> const QTHNF<U>& QTHNF<U>::operator=(const QTHNF<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
  Int_t i;
  QTHN<U>::fNDims=qthn.fNDims;
  QTHN<U>::fAxes=new TAxis*[QTHN<U>::fNDims];
  for(i=0; i<QTHN<U>::fNDims; i++) QTHN<U>::fAxes[i]=qthn.fAxes[i]?(TAxis*)qthn.fAxes[i]->Clone():NULL;
  QTHN<U>::fEntries=qthn.fEntries;
  QTHN<U>::fNBins=qthn.fNBins;
  fNFBins=qthn.fNFBins;
  fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNFBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));
  memcpy(QTHN<U>::fBinContent,qthn.fBinContent,fNFBins*sizeof(U));
  return *this;
}

template <typename U> void QTHNF<U>::Reset()
{
  if(fNFBins) {
    free(fBins); fBins=NULL;
    free(QTHN<U>::fBinContent); QTHN<U>::fBinContent=NULL;
    fNFBins=0;
  }
  QTHN<U>::fEntries=0;
}

template <typename U> QTHN<U>* QTHNF<U>::Projection(const char *name, const Int_t *axes, Int_t naxes, QTHN<U> *th) const
{
  const Int_t nsdims=QTHN<U>::fNDims-naxes;

  if(naxes<=0 || !axes || naxes>=QTHN<U>::fNDims) return NULL;

  Int_t i,j,l,m;

  for(i=0; i<naxes; i++) {
    if(axes[i]<0 || axes[i]>=QTHN<U>::fNDims) {
      fprintf(stderr,"QTHNF<U>::Projection: Error: Invalid axis index: %i\n",axes[i]);
      throw 1;
    }
  }

  if(th) {
    th->SetNDims(naxes);
    th->SetNameTitle(name,name);

  } else th=new QTHNF<U>(name,name,naxes);

  for(i=0; i<naxes; i++) {
    th->SetAxis(i,QTHN<U>::fAxes[axes[i]]);
  }

  Int_t *indices=new Int_t[nsdims]; //Axes indices for axes that are not projected
  Int_t *biniter=new Int_t[QTHN<U>::fNDims]; //Integers used as indices for iteration over bins of original histogram
  Int_t *pbiniter=new Int_t[naxes]; //Integers used as indices for iteration over bins of projected histogram
  U dbuf;
  l=0;

  for(i=0; i<QTHN<U>::fNDims; i++) {

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
      while(biniter[indices[i]]>QTHN<U>::fAxes[indices[i]]->GetNbins()){
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
    while(biniter[axes[i]]>QTHN<U>::fAxes[axes[i]]->GetNbins()){
      biniter[axes[i]]=1;
      i++;

      if(i>=naxes) break;
      biniter[axes[i]]++;
    }

  } while(i<naxes);

  delete[] indices;
  delete[] biniter;
  delete[] pbiniter;

  th->fEntries=QTHN<U>::fEntries;
  return th;
}

template <typename U> void QTHNF<U>::ScaleBinContent(const Long64_t &bin, const Double_t &scale)
{
  Long64_t fbin=GetFBin(bin);
  if(fbin!=-1) QTHN<U>::fBinContent[fbin]*=(U)scale;
}

template <typename U> void QTHNF<U>::ScaleBinContent(const Int_t *coords, const Double_t &scale)
{
  Long64_t fbin=GetFBin(coords);
  if(fbin!=-1) QTHN<U>::fBinContent[fbin]*=(U)scale;
}

template <typename U> void QTHNF<U>::ScaleFBinContent(const Long64_t &fbin, const Double_t &scale)
{
  if(fbin<0 || fbin>=fNFBins) {
    fprintf(stderr,"QTHNF::ScaleFBinContent: Error: Invalid bin index\n");
    throw 1;
  }
  QTHN<U>::fBinContent[fbin]*=(U)scale;
}

template <typename U> void QTHNF<U>::SetBinContent(const Long64_t &bin, const U &content)
{
  Long64_t li;
  Long64_t bidx;

  if(bin<0 || bin>=QTHN<U>::fNBins) {
    fprintf(stderr,"QTHNF::SetBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) {

    if(content) {
      fNFBins++;
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));

      for(li=fNFBins-1; li>bidx; li--) {
	fBins[li]=fBins[li-1];
	QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li-1];
      }
      fBins[bidx]=bin;
      QTHN<U>::fBinContent[bidx]=content;
    }

  } else {

    if(content) {
      QTHN<U>::fBinContent[bidx]=content;

    } else {
      fNFBins--;

      for(Long64_t li=bidx; li<fNFBins; li++) {
	fBins[li]=fBins[li+1];
	QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li+1];
      }
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));
    }
  }
}

template <typename U> void QTHNF<U>::SetBinContent(const Int_t *coords, const U &content)
{
  SetBinContent(GetBin(coords),content);
}

template <typename U> void QTHNF<U>::SetFBinContent(const Long64_t &fbin, const U &content)
{
  if(fbin<0 || fbin>=fNFBins) {
    fprintf(stderr,"QTHNF::SetFBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  if(content) {
    QTHN<U>::fBinContent[fbin]=content;

  } else {
    fNFBins--;

    for(Long64_t li=fbin; li<fNFBins; li++) {
      fBins[li]=fBins[li+1];
      QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li+1];
    }
    fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
    QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));
  }
}

#include "QTHNF_Dict_cxx.h"
