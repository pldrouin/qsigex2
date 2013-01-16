// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QHNF.h"

template <typename U> QHNF<U>::QHNF(const QHNF &qthn): QHN<U>(), fZero(qthn.fZero), fNFBins(qthn.fNFBins), fBins(NULL)
{
  SetNameTitle(qthn.GetName(),qthn.GetTitle());
  Int_t i;
  QHN<U>::fNDims=qthn.fNDims;
  QHN<U>::fAxes=new QAxis*[QHN<U>::fNDims];
  for(i=QHN<U>::fNDims-1; i>=0; --i) QHN<U>::fAxes[i]=qthn.fAxes[i]?(QAxis*)qthn.fAxes[i]->Clone():NULL;
  QHN<U>::fEntries=qthn.fEntries;
  QHN<U>::fNBins=qthn.fNBins;
  fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNFBins*sizeof(Long64_t));
  QHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));
  memcpy(QHN<U>::fBinContent,qthn.fBinContent,fNFBins*sizeof(U));
  QHN<U>::fIntUseFBinLoop=kTRUE;
}

template <typename U> QHNF<U>::QHNF(const QHN<U> &qthn): QHN<U>(), fZero(0), fNFBins(0), fBins(NULL)
{
  SetNameTitle(qthn.GetName(),qthn.GetTitle());
  QHN<U>::fNDims=qthn.GetNDims();
  QHN<U>::fAxes=new QAxis*[QHN<U>::fNDims];
  for(Int_t i=QHN<U>::fNDims-1; i>=0; --i) QHN<U>::fAxes[i]=qthn.GetAxis(i)?(QAxis*)qthn.GetAxis(i)->Clone():NULL;
  QHN<U>::fEntries=qthn.GetEntries();
  QHN<U>::fNBins=qthn.GetNBins();
  fBins=(Long64_t*)malloc(QHN<U>::fNBins*sizeof(Long64_t));
  QHN<U>::fBinContent=(U*)malloc(QHN<U>::fNBins*sizeof(U));

  for(Long64_t li=QHN<U>::fNBins-1; li>=0; --li) {

    if(qthn.GetBinContent(li)) {
      fBins[fNFBins]=li;
      QHN<U>::fBinContent[fNFBins]=qthn.GetBinContent(li);
      ++fNFBins;
    }
  }
  fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
  QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,fNFBins*sizeof(U));
  QHN<U>::fIntUseFBinLoop=kTRUE;
}

template <typename U> void QHNF<U>::AddBinContent(const Long64_t &bin, const U &w)
{
#ifndef QSFAST
      if(bin<0 || bin>=QHN<U>::fNBins) {
	fprintf(stderr,"Error: QHNF::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif

  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) {

    if(w) {
      ++fNFBins;
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,fNFBins*sizeof(U));

      for(Long64_t li=fNFBins-1; li>bidx; --li) {
	fBins[li]=fBins[li-1];
	QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li-1];
      }
      fBins[bidx]=bin;
      QHN<U>::fBinContent[bidx]=w;
    }

  } else {
    QHN<U>::fBinContent[bidx]+=w;
  }
  QHN<U>::fEntries+=w;
}

template <typename U> void QHNF<U>::Clear(Option_t* option)
{
  QHN<U>::Clear(option);

  if(fNFBins) {
    free(fBins);
  }
  fNFBins=0;
  fBins=NULL;
}

template <typename U> void QHNF<U>::ComputeNBins()
{
  QHN<U>::fNBins=1;
  for(Int_t i=QHN<U>::fNDims-1; i>=0; --i) if(QHN<U>::fAxes[i]) QHN<U>::fNBins*=QHN<U>::fAxes[i]->GetNBins()+2;
}

template <typename U> void QHNF<U>::CopyStruct(const QHN<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
  QHN<U>::fNDims=qthn.GetNDims();
  QHN<U>::fAxes=new QAxis*[QHN<U>::fNDims];
  for(Int_t i=QHN<U>::fNDims-1; i>=0; --i) QHN<U>::fAxes[i]=qthn.GetAxis(i)?(QAxis*)qthn.GetAxis(i)->Clone():NULL;
  QHN<U>::fNBins=qthn.GetNBins();
  fNFBins=qthn.GetNFbins();
  fBins=(Long64_t*)malloc(qthn.GetNFbins()*sizeof(Long64_t));
  QHN<U>::fBinContent=(U*)malloc(qthn.GetNFbins()*sizeof(U));
  const QHNF<U> *qhnf=dynamic_cast<const QHNF<U>*>(&qthn);

  if(qhnf) memcpy(fBins,qhnf->fBins,fNFBins*sizeof(Long64_t));
  else for(Long64_t li=fNFBins-1; li>=0; --li) fBins[li]=li;
  memset(QHN<U>::fBinContent,0,fNFBins*sizeof(U));
}

template <typename U> void QHNF<U>::Init()
{
  QHN<U>::fNBins=1;
  for(Int_t i=QHN<U>::fNDims-1; i>=0; --i) QHN<U>::fNBins*=QHN<U>::fAxes[i]->GetNBins()+2;
  QHN<U>::fBinContent=NULL;
  QHN<U>::fEntries=0;
  QHN<U>::fIntUseFBinLoop=kTRUE;
  QHN<U>::fTH=NULL;
  fNFBins=0;
  fBins=NULL;
}

template <typename U> inline Long64_t QHNF<U>::GetFBin(const Int_t *coords) const
{
  Long64_t bin=QHN<U>::GetBin(coords);
  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) return -1;
  return bidx;
}

template <typename U> inline Long64_t QHNF<U>::GetFBin(const Long64_t &bin) const
{
  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) return -1;
  return bidx;
}

template <typename U> inline const U& QHNF<U>::GetBinContent(const Long64_t &bin) const
{
  Long64_t li=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(li==fNFBins || fBins[li]!=bin) return fZero;
  return QHN<U>::fBinContent[li];
}

template <typename U> inline void QHNF<U>::GetFBinCoords(const Long64_t &fbin, Int_t *coords) const
{
  Long64_t bin=fBins[fbin];
  coords[0]=bin%(QHN<U>::fAxes[0]->GetNBins()+2);

  for(Int_t i=1; i<QHN<U>::fNDims; ++i) {
    bin=(bin-coords[i-1])/(QHN<U>::fAxes[i-1]->GetNBins()+2);
    coords[i]=bin%(QHN<U>::fAxes[i]->GetNBins()+2);
  }
}

template <typename U> inline Bool_t QHNF<U>::IsFBinIncluded(const Long64_t &fbin, const Int_t *mins, const Int_t *maxs) const
{
  Long64_t bin=fBins[fbin];
  Int_t coord=bin%(QHN<U>::fAxes[0]->GetNBins()+2);
  if(coord<mins[0] || coord>maxs[0]) return kFALSE;

  for(Int_t i=1; i<QHN<U>::fNDims; ++i) {
    bin=(bin-coord)/(QHN<U>::fAxes[i-1]->GetNBins()+2);
    coord=bin%(QHN<U>::fAxes[i]->GetNBins()+2);
    if(coord<mins[i] || coord>maxs[i]) return kFALSE;
  }
  return kTRUE;
}

template <typename U> const QHNF<U>& QHNF<U>::operator=(const QHNF<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
  QHN<U>::fNDims=qthn.fNDims;
  QHN<U>::fAxes=new QAxis*[QHN<U>::fNDims];
  for(Int_t i=QHN<U>::fNDims-1; i>=0; --i) QHN<U>::fAxes[i]=qthn.fAxes[i]?(QAxis*)qthn.fAxes[i]->Clone():NULL;
  QHN<U>::fEntries=qthn.fEntries;
  QHN<U>::fNBins=qthn.fNBins;
  fNFBins=qthn.fNFBins;
  fBins=(Long64_t*)malloc(fNFBins*sizeof(Long64_t));
  memcpy(fBins,qthn.fBins,fNFBins*sizeof(Long64_t));
  QHN<U>::fBinContent=(U*)malloc(fNFBins*sizeof(U));
  memcpy(QHN<U>::fBinContent,qthn.fBinContent,fNFBins*sizeof(U));
  return *this;
}

template <typename U> const QHNF<U>& QHNF<U>::operator=(const QHN<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
  QHN<U>::fNDims=qthn.GetNDims();
  QHN<U>::fAxes=new QAxis*[QHN<U>::fNDims];
  for(Int_t i=QHN<U>::fNDims-1; i>=0; --i) QHN<U>::fAxes[i]=qthn.GetAxis(i)?(QAxis*)qthn.GetAxis(i)->Clone():NULL;
  QHN<U>::fEntries=qthn.GetEntries();
  QHN<U>::fNBins=qthn.GetNBins();
  fBins=(Long64_t*)malloc(qthn.GetNFbins()*sizeof(Long64_t));
  QHN<U>::fBinContent=(U*)malloc(qthn.GetNFbins()*sizeof(U));

  for(Long64_t li=QHN<U>::GetNFbins()-1; li>=0; --li) {

    if(qthn.GetFBinContent(li)) {
      fBins[fNFBins]=qthn.GetFBinCoord(li);
      QHN<U>::fBinContent[fNFBins]=qthn.GetFBinContent(li);
      ++fNFBins;
    }
  }
  fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
  QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,fNFBins*sizeof(U));

  return *this;
}

template <typename U> inline void QHNF<U>::Reset()
{
  if(fNFBins) {
    free(fBins); fBins=NULL;
    free(QHN<U>::fBinContent); QHN<U>::fBinContent=NULL;
    fNFBins=0;
  }

  if(QHN<U>::fTH) {
    delete QHN<U>::fTH;
    QHN<U>::fTH=NULL;
  }
  QHN<U>::fEntries=0;
}

template <typename U> inline void QHNF<U>::ScaleBinContent(const Long64_t &bin, const Double_t &scale)
{
  Long64_t fbin=GetFBin(bin);
  if(fbin!=-1) QHN<U>::fBinContent[fbin]*=(U)scale;
}

template <typename U> inline void QHNF<U>::ScaleBinContent(const Int_t *coords, const Double_t &scale)
{
  Long64_t fbin=GetFBin(coords);
  if(fbin!=-1) QHN<U>::fBinContent[fbin]*=(U)scale;
}

template <typename U> void QHNF<U>::SetBinContent(const Long64_t &bin, const U &content)
{
  Long64_t bidx=std::lower_bound(fBins, fBins+fNFBins, bin)-fBins;

  if(bidx==fNFBins || fBins[bidx]!=bin) {

    if(content) {
      ++fNFBins;
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,fNFBins*sizeof(U));

      for(Long64_t li=fNFBins-1; li>bidx; --li) {
	fBins[li]=fBins[li-1];
	QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li-1];
      }
      fBins[bidx]=bin;
      QHN<U>::fBinContent[bidx]=content;
    }

  } else {

    if(content) {
      QHN<U>::fBinContent[bidx]=content;

    } else {
      fNFBins--;

      for(Long64_t li=bidx; li<fNFBins; ++li) {
	fBins[li]=fBins[li+1];
	QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li+1];
      }
      fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
      QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,fNFBins*sizeof(U));
    }
  }
}

template <typename U> inline void QHNF<U>::SetBinContent(const Int_t *coords, const U &content)
{
  SetBinContent(QHN<U>::GetBin(coords),content);
}

template <typename U> void QHNF<U>::SetFBinContent(const Long64_t &fbin, const U &content)
{
  if(content) {
    QHN<U>::fBinContent[fbin]=content;

  } else {
    fNFBins--;

    for(Long64_t li=fbin; li<fNFBins; ++li) {
      fBins[li]=fBins[li+1];
      QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li+1];
    }
    fBins=(Long64_t*)realloc(fBins,fNFBins*sizeof(Long64_t));
    QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,fNFBins*sizeof(U));
  }
}

#include "QHNF_Dict_cxx.h"
