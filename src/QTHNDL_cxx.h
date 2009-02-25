// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#include "QTHNDL.h"

template <typename U> QTHNDL<U>::QTHNDL(const QTHNDL &qthn): QTHNF<U>(qthn), fFBins(NULL)
{
  fFBins=(Long64_t*)malloc(QTHN<U>::fNBins*sizeof(Long64_t));
  memcpy(fFBins,qthn.fFBins,QTHN<U>::fNBins*sizeof(Long64_t));
}

template <typename U> void QTHNDL<U>::AddBinContent(const Long64_t &bin, const U &w)
{

  if(bin<0 || bin>=QTHN<U>::fNBins) {
    fprintf(stderr,"QTHNDL::AddBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  if(fFBins[bin]!=-1) {
    QTHN<U>::fBinContent[fFBins[bin]]+=w;
    QTHN<U>::fEntries++;

  } else {
    Long64_t li;
    Long64_t bidx;

    bidx=std::lower_bound(QTHNF<U>::fBins, QTHNF<U>::fBins+QTHNF<U>::fNFBins, bin)-QTHNF<U>::fBins;

    if(bidx==QTHNF<U>::fNFBins || QTHNF<U>::fBins[bidx]!=bin) {

      if(w) {
	QTHNF<U>::fNFBins++;
	QTHNF<U>::fBins=(Long64_t*)realloc(QTHNF<U>::fBins,QTHNF<U>::fNFBins*sizeof(Long64_t));
	QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,QTHNF<U>::fNFBins*sizeof(U));

	for(li=QTHNF<U>::fNFBins-1; li>bidx; li--) {
          fFBins[QTHNF<U>::fBins[li-1]]=li;
	  QTHNF<U>::fBins[li]=QTHNF<U>::fBins[li-1];
	  QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li-1];
	}
	QTHNF<U>::fBins[bidx]=bin;
	fFBins[bin]=bidx;
	QTHN<U>::fBinContent[bidx]=w;
      }

    } else {
      QTHN<U>::fBinContent[bidx]+=w;
    }
    QTHN<U>::fEntries++;
  }
}

template <typename U> void QTHNDL<U>::Clear(Option_t* option)
{
  QTHNF<U>::Clear(option);
  if(fFBins) free(fFBins);
  fFBins=NULL;
}

template <typename U> Long64_t QTHNDL<U>::GetFBin(const Long64_t &bin) const
{

  if(bin<0 || bin>=QTHN<U>::fNBins) {
    fprintf(stderr,"QTHNDL::GetFBin: Error: Invalid bin index\n");
    throw 1;
  }
  return fFBins[bin];
}

template <typename U> const U& QTHNDL<U>::GetBinContent(const Int_t *coords) const
{
  Long64_t bin=QTHN<U>::GetBin(coords);
  return GetBinContent(bin);
}

template <typename U> const U& QTHNDL<U>::GetBinContent(const Long64_t &bin) const
{

  if(bin<0 || bin>=QTHN<U>::fNBins) {
    fprintf(stderr,"QTHNDL::GetFBin: Error: Invalid bin index\n");
    throw 1;
  }

  if(fFBins[bin]!=-1) return QTHN<U>::fBinContent[fFBins[bin]];
  else return QTHNF<U>::fZero;
}

template <typename U> const QTHNDL<U>& QTHNDL<U>::operator=(const QTHNDL<U> &qthn)
{
  Clear();
  QTHNF<U>::operator=(qthn);
  fFBins=(Long64_t*)malloc(QTHN<U>::fNBins*sizeof(Long64_t));
  memcpy(fFBins,qthn.fFBins,QTHN<U>::fNBins*sizeof(Long64_t));
  return *this;
}

template <typename U> QTHN<U>* QTHNDL<U>::Projection(const char *name, const Int_t *axes, Int_t naxes, QTHN<U> *th) const
{
  if(th) {
    th->SetNDims(naxes);
    th->SetNameTitle(name,name);

  } else th=new QTHNDL<U>(name,name,naxes);
  return QTHN<U>::Projection(name,axes,naxes,th);
}

template <typename U> void QTHNDL<U>::Reset()
{
  QTHNF<U>::Reset();

  for(Long64_t li=0; li<QTHN<U>::fNBins; li++) fFBins[li]=-1;
}

template <typename U> void QTHNDL<U>::SetBinContent(const Long64_t &bin, const U &content)
{

  if(bin<0 || bin>=QTHN<U>::fNBins) {
    fprintf(stderr,"QTHNDL::SetBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  if(fFBins[bin]!=-1) {

    if(content) {
      QTHN<U>::fBinContent[fFBins[bin]]=content;

    } else {
      Long64_t bidx=fFBins[bin];
      QTHNF<U>::fNFBins--;
      fFBins[bin]=-1;

      for(Long64_t li=bidx; li<QTHNF<U>::fNFBins; li++) {
        fFBins[QTHNF<U>::fBins[li+1]]=li;
	QTHNF<U>::fBins[li]=QTHNF<U>::fBins[li+1];
	QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li+1];
      }
      QTHNF<U>::fBins=(Long64_t*)realloc(QTHNF<U>::fBins,QTHNF<U>::fNFBins*sizeof(Long64_t));
      QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,QTHNF<U>::fNFBins*sizeof(U));
    }

  } else {
    Long64_t li;
    Long64_t bidx;

    bidx=std::lower_bound(QTHNF<U>::fBins, QTHNF<U>::fBins+QTHNF<U>::fNFBins, bin)-QTHNF<U>::fBins;

    if(content) {
      QTHNF<U>::fNFBins++;
      QTHNF<U>::fBins=(Long64_t*)realloc(QTHNF<U>::fBins,QTHNF<U>::fNFBins*sizeof(Long64_t));
      QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,QTHNF<U>::fNFBins*sizeof(U));

      for(li=QTHNF<U>::fNFBins-1; li>bidx; li--) {
        fFBins[QTHNF<U>::fBins[li-1]]=li;
	QTHNF<U>::fBins[li]=QTHNF<U>::fBins[li-1];
	QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li-1];
      }
      QTHNF<U>::fBins[bidx]=bin;
      fFBins[bin]=bidx;
      QTHN<U>::fBinContent[bidx]=content;
    }
  }
}

template <typename U> void QTHNDL<U>::SetFBinContent(const Long64_t &fbin, const U &content)
{
  if(fbin<0 || fbin>=QTHNF<U>::fNFBins) {
    fprintf(stderr,"QTHNDL::SetFBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  if(content) {
    QTHN<U>::fBinContent[fbin]=content;

  } else {
    fFBins[QTHNF<U>::fBins[fbin]]=-1;
    QTHNF<U>::fNFBins--;

    for(Long64_t li=fbin; li<QTHNF<U>::fNFBins; li++) {
      fFBins[QTHNF<U>::fBins[li+1]]=li;
      QTHNF<U>::fBins[li]=QTHNF<U>::fBins[li+1];
      QTHN<U>::fBinContent[li]=QTHN<U>::fBinContent[li+1];
    }
    QTHNF<U>::fBins=(Long64_t*)realloc(QTHNF<U>::fBins,QTHNF<U>::fNFBins*sizeof(Long64_t));
    QTHN<U>::fBinContent=(U*)realloc(QTHN<U>::fBinContent,QTHNF<U>::fNFBins*sizeof(U));
  }
}

template <typename U> void QTHNDL<U>::ComputeNBins()
{
  QTHNF<U>::ComputeNBins();
  Long64_t li;

  if(fFBins) free(fFBins);
  fFBins=(Long64_t*)malloc(QTHN<U>::fNBins*sizeof(Long64_t));

  for(li=0; li<QTHN<U>::fNBins; li++) fFBins[li]=-1;

  for(li=0; li<QTHNF<U>::fNFBins; li++) fFBins[QTHNF<U>::fBins[li]]=li;
}

#include "QTHNDL_Dict_cxx.h"
