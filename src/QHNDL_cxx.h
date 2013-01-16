// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QHNDL.h"

template <typename U> QHNDL<U>::QHNDL(const QHNDL &qthn): QHNF<U>(qthn), fFBins(NULL)
{
  fFBins=(Long64_t*)malloc(QHN<U>::fNBins*sizeof(Long64_t));
  memcpy(fFBins,qthn.fFBins,QHN<U>::fNBins*sizeof(Long64_t));
  QHN<U>::fIntUseFBinLoop=kFALSE;
}

template <typename U> void QHNDL<U>::AddBinContent(const Long64_t &bin, const U &w)
{
#ifndef QSFAST
      if(bin<0 || bin>=QHN<U>::fNBins) {
	fprintf(stderr,"Error: QHNDL::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif

  if(fFBins[bin]!=-1) {
    QHN<U>::fBinContent[fFBins[bin]]+=w;
    QHN<U>::fEntries+=w;

  } else {
    Long64_t bidx=std::lower_bound(QHNF<U>::fBins, QHNF<U>::fBins+QHNF<U>::fNFBins, bin)-QHNF<U>::fBins;

    if(bidx==QHNF<U>::fNFBins || QHNF<U>::fBins[bidx]!=bin) {

      if(w) {
	++QHNF<U>::fNFBins;
	QHNF<U>::fBins=(Long64_t*)realloc(QHNF<U>::fBins,QHNF<U>::fNFBins*sizeof(Long64_t));
	QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,QHNF<U>::fNFBins*sizeof(U));

	for(Long64_t li=QHNF<U>::fNFBins-1; li>bidx; --li) {
          fFBins[QHNF<U>::fBins[li-1]]=li;
	  QHNF<U>::fBins[li]=QHNF<U>::fBins[li-1];
	  QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li-1];
	}
	QHNF<U>::fBins[bidx]=bin;
	fFBins[bin]=bidx;
	QHN<U>::fBinContent[bidx]=w;
      }

    } else {
      QHN<U>::fBinContent[bidx]+=w;
    }
    QHN<U>::fEntries+=w;
  }
}

template <typename U> void QHNDL<U>::Clear(Option_t* option)
{
  QHNF<U>::Clear(option);
  if(fFBins) free(fFBins);
  fFBins=NULL;
}

template <typename U> void QHNDL<U>::CopyStruct(const QHN<U> &qthn)
{
  QHNF<U>::CopyStruct(qthn);
  const QHNDL<U> *qhndl=dynamic_cast<const QHNDL<U>*>(&qthn);

  if(qhndl) {
    fFBins=(Long64_t*)malloc(QHN<U>::fNBins*sizeof(Long64_t));
    memcpy(fFBins,qhndl->fFBins,QHN<U>::fNBins*sizeof(Long64_t));
  } else ComputeNBins();
}

template <typename U> inline const U& QHNDL<U>::GetBinContent(const Long64_t &bin) const
{
  if(fFBins[bin]!=-1) return QHN<U>::fBinContent[fFBins[bin]];
  else return QHNF<U>::fZero;
}

template <typename U> const QHNDL<U>& QHNDL<U>::operator=(const QHNDL<U> &qthn)
{
  QHNF<U>::operator=(qthn);
  fFBins=(Long64_t*)malloc(QHN<U>::fNBins*sizeof(Long64_t));
  memcpy(fFBins,qthn.fFBins,QHN<U>::fNBins*sizeof(Long64_t));
  return *this;
}

template <typename U> void QHNDL<U>::Reset()
{
  QHNF<U>::Reset();

  for(Long64_t li=QHN<U>::fNBins-1; li>=0; --li) fFBins[li]=-1;
}

template <typename U> void QHNDL<U>::SetBinContent(const Long64_t &bin, const U &content)
{
  if(fFBins[bin]!=-1) {

    if(content) {
      QHN<U>::fBinContent[fFBins[bin]]=content;

    } else {
      Long64_t bidx=fFBins[bin];
      QHNF<U>::fNFBins--;
      fFBins[bin]=-1;

      for(Long64_t li=bidx; li<QHNF<U>::fNFBins; ++li) {
        fFBins[QHNF<U>::fBins[li+1]]=li;
	QHNF<U>::fBins[li]=QHNF<U>::fBins[li+1];
	QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li+1];
      }
      QHNF<U>::fBins=(Long64_t*)realloc(QHNF<U>::fBins,QHNF<U>::fNFBins*sizeof(Long64_t));
      QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,QHNF<U>::fNFBins*sizeof(U));
    }

  } else {
    Long64_t bidx=std::lower_bound(QHNF<U>::fBins, QHNF<U>::fBins+QHNF<U>::fNFBins, bin)-QHNF<U>::fBins;

    if(content) {
      ++QHNF<U>::fNFBins;
      QHNF<U>::fBins=(Long64_t*)realloc(QHNF<U>::fBins,QHNF<U>::fNFBins*sizeof(Long64_t));
      QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,QHNF<U>::fNFBins*sizeof(U));

      for(Long64_t li=QHNF<U>::fNFBins-1; li>bidx; --li) {
        fFBins[QHNF<U>::fBins[li-1]]=li;
	QHNF<U>::fBins[li]=QHNF<U>::fBins[li-1];
	QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li-1];
      }
      QHNF<U>::fBins[bidx]=bin;
      fFBins[bin]=bidx;
      QHN<U>::fBinContent[bidx]=content;
    }
  }
}

template <typename U> void QHNDL<U>::SetFBinContent(const Long64_t &fbin, const U &content)
{
  if(content) {
    QHN<U>::fBinContent[fbin]=content;

  } else {
    fFBins[QHNF<U>::fBins[fbin]]=-1;
    QHNF<U>::fNFBins--;

    for(Long64_t li=fbin; li<QHNF<U>::fNFBins; ++li) {
      fFBins[QHNF<U>::fBins[li+1]]=li;
      QHNF<U>::fBins[li]=QHNF<U>::fBins[li+1];
      QHN<U>::fBinContent[li]=QHN<U>::fBinContent[li+1];
    }
    QHNF<U>::fBins=(Long64_t*)realloc(QHNF<U>::fBins,QHNF<U>::fNFBins*sizeof(Long64_t));
    QHN<U>::fBinContent=(U*)realloc(QHN<U>::fBinContent,QHNF<U>::fNFBins*sizeof(U));
  }
}

template <typename U> void QHNDL<U>::ComputeNBins()
{
  QHNF<U>::ComputeNBins();
  Long64_t li;

  if(fFBins) free(fFBins);
  fFBins=(Long64_t*)malloc(QHN<U>::fNBins*sizeof(Long64_t));

  for(li=QHN<U>::fNBins-1; li>=0; --li) fFBins[li]=-1;

  for(li=QHNF<U>::fNFBins-1; li>=0; --li) fFBins[QHNF<U>::fBins[li]]=li;
}

template <typename U> void QHNDL<U>::Init()
{
  Long64_t li;
  fFBins=(Long64_t*)malloc(QHN<U>::fNBins*sizeof(Long64_t));

  for(li=QHN<U>::fNBins-1; li>=0; --li) fFBins[li]=-1;
  QHN<U>::fIntUseFBinLoop=kFALSE;
}

#include "QHNDL_Dict_cxx.h"
