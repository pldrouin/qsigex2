// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#include "QTHNDL.h"

template <typename U> QTHNDL<U>::QTHNDL(const QTHNDL &qthn): QTHN<U>(qthn), fFBins(NULL)
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
    QTHN<U>::fFBinContent[fFBins[bin]]+=w;
    QTHN<U>::fEntries++;

  } else {
    Long64_t li;
    Long64_t bidx;

    bidx=std::lower_bound(QTHN<U>::fBins, QTHN<U>::fBins+QTHN<U>::fNFBins, bin)-QTHN<U>::fBins;

    if(bidx==QTHN<U>::fNFBins || QTHN<U>::fBins[bidx]!=bin) {

      if(w) {
	QTHN<U>::fNFBins++;
	QTHN<U>::fBins=(Long64_t*)realloc(QTHN<U>::fBins,QTHN<U>::fNFBins*sizeof(Long64_t));
	QTHN<U>::fFBinContent=(U*)realloc(QTHN<U>::fFBinContent,QTHN<U>::fNFBins*sizeof(U));

	for(li=QTHN<U>::fNFBins-1; li>bidx; li--) {
          fFBins[QTHN<U>::fBins[li-1]]=li;
	  QTHN<U>::fBins[li]=QTHN<U>::fBins[li-1];
	  QTHN<U>::fFBinContent[li]=QTHN<U>::fFBinContent[li-1];
	}
	QTHN<U>::fBins[bidx]=bin;
	fFBins[bin]=bidx;
	QTHN<U>::fFBinContent[bidx]=w;
      }

    } else {
      QTHN<U>::fFBinContent[bidx]+=w;
    }
    QTHN<U>::fEntries++;
  }
}

template <typename U> void QTHNDL<U>::Clear(Option_t* option)
{
  QTHN<U>::Clear(option);
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

  if(fFBins[bin]!=-1) return QTHN<U>::fFBinContent[fFBins[bin]];
  else return QTHN<U>::fZero;
}

template <typename U> const QTHNDL<U>& QTHNDL<U>::operator=(const QTHNDL<U> &qthn)
{
  Clear();
  QTHN<U>::operator=(qthn);
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
  QTHN<U>::Reset();

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
      QTHN<U>::fFBinContent[fFBins[bin]]=content;

    } else {
      Long64_t bidx=fFBins[bin];
      QTHN<U>::fNFBins--;
      fFBins[bin]=-1;

      for(Long64_t li=bidx; li<QTHN<U>::fNFBins; li++) {
        fFBins[QTHN<U>::fBins[li+1]]=li;
	QTHN<U>::fBins[li]=QTHN<U>::fBins[li+1];
	QTHN<U>::fFBinContent[li]=QTHN<U>::fFBinContent[li+1];
      }
      QTHN<U>::fBins=(Long64_t*)realloc(QTHN<U>::fBins,QTHN<U>::fNFBins*sizeof(Long64_t));
      QTHN<U>::fFBinContent=(U*)realloc(QTHN<U>::fFBinContent,QTHN<U>::fNFBins*sizeof(U));
    }

  } else {
    Long64_t li;
    Long64_t bidx;

    bidx=std::lower_bound(QTHN<U>::fBins, QTHN<U>::fBins+QTHN<U>::fNFBins, bin)-QTHN<U>::fBins;

    if(content) {
      QTHN<U>::fNFBins++;
      QTHN<U>::fBins=(Long64_t*)realloc(QTHN<U>::fBins,QTHN<U>::fNFBins*sizeof(Long64_t));
      QTHN<U>::fFBinContent=(U*)realloc(QTHN<U>::fFBinContent,QTHN<U>::fNFBins*sizeof(U));

      for(li=QTHN<U>::fNFBins-1; li>bidx; li--) {
        fFBins[QTHN<U>::fBins[li-1]]=li;
	QTHN<U>::fBins[li]=QTHN<U>::fBins[li-1];
	QTHN<U>::fFBinContent[li]=QTHN<U>::fFBinContent[li-1];
      }
      QTHN<U>::fBins[bidx]=bin;
      fFBins[bin]=bidx;
      QTHN<U>::fFBinContent[bidx]=content;
    }
  }
}

template <typename U> void QTHNDL<U>::SetFBinContent(const Long64_t &fbin, const U &content)
{
  if(fbin<0 || fbin>=QTHN<U>::fNFBins) {
    fprintf(stderr,"QTHNDL::SetFBinContent: Error: Invalid bin index\n");
    throw 1;
  }

  if(content) {
    QTHN<U>::fFBinContent[fbin]=content;

  } else {
    fFBins[QTHN<U>::fBins[fbin]]=-1;
    QTHN<U>::fNFBins--;

    for(Long64_t li=fbin; li<QTHN<U>::fNFBins; li++) {
      fFBins[QTHN<U>::fBins[li+1]]=li;
      QTHN<U>::fBins[li]=QTHN<U>::fBins[li+1];
      QTHN<U>::fFBinContent[li]=QTHN<U>::fFBinContent[li+1];
    }
    QTHN<U>::fBins=(Long64_t*)realloc(QTHN<U>::fBins,QTHN<U>::fNFBins*sizeof(Long64_t));
    QTHN<U>::fFBinContent=(U*)realloc(QTHN<U>::fFBinContent,QTHN<U>::fNFBins*sizeof(U));
  }
}

template <typename U> void QTHNDL<U>::ComputeNBins()
{
  QTHN<U>::ComputeNBins();
  Long64_t li;

  if(fFBins) free(fFBins);
  fFBins=(Long64_t*)malloc(QTHN<U>::fNBins*sizeof(Long64_t));

  for(li=0; li<QTHN<U>::fNBins; li++) fFBins[li]=-1;

  for(li=0; li<QTHN<U>::fNFBins; li++) fFBins[QTHN<U>::fBins[li]]=li;
}

#include "QTHNDL_Dict_cxx.h"
