// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#include "QTHNF.h"

template <typename U> QTHNF<U>::QTHNF(const QTHNF &qthn): QTHN<U>(), fNFBins(qthn.fNFBins), fBins(NULL), fZero(qthn.fZero)
{
  SetNameTitle(qthn.GetName(),qthn.GetTitle());
  Int_t i;
  QTHN<U>::fNDims=qthn.fNDims;
  QTHN<U>::fAxes=new QAxis*[QTHN<U>::fNDims];
  for(i=QTHN<U>::fNDims-1; i>=0; --i) QTHN<U>::fAxes[i]=qthn.fAxes[i]?(QAxis*)qthn.fAxes[i]->Clone():NULL;
  QTHN<U>::fEntries=qthn.fEntries;
  QTHN<U>::fNBins=qthn.fNBins;
  fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNFBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));
  memcpy(QTHN<U>::fBinContent,qthn.fBinContent,fNFBins*sizeof(U));
}

template <typename U> QTHNF<U>::QTHNF(const QTHN<U> &qthn): QTHN<U>(), fNFBins(0), fBins(NULL), fZero(0)
{
  SetNameTitle(qthn.GetName(),qthn.GetTitle());
  QTHN<U>::fNDims=qthn.GetNDims();
  QTHN<U>::fAxes=new QAxis*[QTHN<U>::fNDims];
  for(Int_t i=QTHN<U>::fNDims-1; i>=0; --i) QTHN<U>::fAxes[i]=qthn.GetAxis(i)?(QAxis*)qthn.GetAxis(i)->Clone():NULL;
  QTHN<U>::fEntries=qthn.GetEntries();
  QTHN<U>::fNBins=qthn.GetNBins();
  fBins=(Long64_t*)malloc(QTHN<U>::fNBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)malloc(QTHN<U>::fNBins*sizeof(U));

  for(Long64_t li=QTHN<U>::fNBins-1; li>=0; --li) {

    if(qthn.GetBinContent(li)) {
      fBins[fNFBins]=li;
      QTHN<U>::fBinContent[fNFBins]=qthn.GetBinContent(li);
      fNFBins++;
    }
  }
  fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));
}

template <typename U> void QTHNF<U>::AddBinContent(const Long64_t &bin, const U &w)
{
  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) {

    if(w) {
      fNFBins++;
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));

      for(Long64_t li=fNFBins-1; li>bidx; --li) {
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
  QTHN<U>::fNBins=1;
  for(Int_t i=QTHN<U>::fNDims-1; i>=0; --i) if(QTHN<U>::fAxes[i]) QTHN<U>::fNBins*=QTHN<U>::fAxes[i]->GetNBins()+2;
}

template <typename U> Long64_t QTHNF<U>::GetFBin(const Int_t *coords) const
{
  Long64_t bin=QTHN<U>::GetBin(coords);
  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

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

template <typename U> void QTHNF<U>::GetFBinCoords(const Long64_t &fbin, Int_t *coords) const
{
  Long64_t bin=fBins[fbin];
  coords[0]=bin%(QTHN<U>::fAxes[0]->GetNBins()+2);

  for(Int_t i=1; i<QTHN<U>::fNDims; i++) {
    bin=(bin-coords[i-1])/(QTHN<U>::fAxes[i-1]->GetNBins()+2);
    coords[i]=bin%(QTHN<U>::fAxes[i]->GetNBins()+2);
  }
}

template <typename U> Bool_t QTHNF<U>::IsFBinIncluded(const Long64_t &fbin, const Int_t *mins, const Int_t *maxs) const
{
  Long64_t bin=fBins[fbin];
  Int_t coord=bin%(QTHN<U>::fAxes[0]->GetNBins()+2);
  if(coord<mins[0] || coord>maxs[0]) return kFALSE;

  for(Int_t i=1; i<QTHN<U>::fNDims; i++) {
    bin=(bin-coord)/(QTHN<U>::fAxes[i-1]->GetNBins()+2);
    coord=bin%(QTHN<U>::fAxes[i]->GetNBins()+2);
    if(coord<mins[i] || coord>maxs[i]) return kFALSE;
  }
  return kTRUE;
}

template <typename U> const QTHNF<U>& QTHNF<U>::operator=(const QTHNF<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
  QTHN<U>::fNDims=qthn.fNDims;
  QTHN<U>::fAxes=new QAxis*[QTHN<U>::fNDims];
  for(Int_t i=QTHN<U>::fNDims-1; i>=0; --i) QTHN<U>::fAxes[i]=qthn.fAxes[i]?(QAxis*)qthn.fAxes[i]->Clone():NULL;
  QTHN<U>::fEntries=qthn.fEntries;
  QTHN<U>::fNBins=qthn.fNBins;
  fNFBins=qthn.fNFBins;
  fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNFBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));
  memcpy(QTHN<U>::fBinContent,qthn.fBinContent,fNFBins*sizeof(U));
  return *this;
}

template <typename U> const QTHNF<U>& QTHNF<U>::operator=(const QTHN<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
  QTHN<U>::fNDims=qthn.GetNDims();
  QTHN<U>::fAxes=new QAxis*[QTHN<U>::fNDims];
  for(Int_t i=QTHN<U>::fNDims-1; i>=0; --i) QTHN<U>::fAxes[i]=qthn.GetAxis(i)?(QAxis*)qthn.GetAxis(i)->Clone():NULL;
  QTHN<U>::fEntries=qthn.GetEntries();
  QTHN<U>::fNBins=qthn.GetNBins();
  fBins=(Long64_t*)malloc(QTHN<U>::fNBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)malloc(QTHN<U>::fNBins*sizeof(U));

  for(Long64_t li=QTHN<U>::fNBins-1; li>=0; --li) {

    if(qthn.GetBinContent(li)) {
      fBins[fNFBins]=li;
      QTHN<U>::fBinContent[fNFBins]=qthn.GetBinContent(li);
      fNFBins++;
    }
  }
  fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
  QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));

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

template <typename U> QTHN<U>* QTHNF<U>::Projection(const char *name, const Int_t *axes, const Int_t &naxes, QTHN<U> *th) const
{
  if(th) {
    th->SetNDims(naxes);
    th->SetNameTitle(name,name);

  } else th=new QTHNF<U>(name,name,naxes);
  return QTHN<U>::Projection(name,axes,naxes,th);
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

template <typename U> void QTHNF<U>::SetBinContent(const Long64_t &bin, const U &content)
{
  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) {

    if(content) {
      fNFBins++;
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));

      for(Long64_t li=fNFBins-1; li>bidx; --li) {
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

      for(Long64_t li=bidx; li<fNFBins; ++li) {
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
  SetBinContent(QTHN<U>::GetBin(coords),content);
}

template <typename U> void QTHNF<U>::SetFBinContent(const Long64_t &fbin, const U &content)
{
  if(content) {
    QTHN<U>::fBinContent[fbin]=content;

  } else {
    fNFBins--;

    for(Long64_t li=fbin; li<fNFBins; ++li) {
      fBins[li]=fBins[li+1];
      QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li+1];
    }
    fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
    QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,fNFBins*sizeof(U));
  }
}

#include "QTHNF_Dict_cxx.h"
