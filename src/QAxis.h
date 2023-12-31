// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QAXIS_
#define _QAXIS_

#include <algorithm>
#include "TNamed.h"
#include "TAxis.h"
#include "TBuffer.h"

class QAxis: public TNamed
{
  public:
    QAxis(): TNamed(), fNBins(1), fMin(0), fMax(1), fBWidth(1), fBins(NULL){}
    QAxis(const Int_t &nbins, const Double_t& min, const Double_t& max): TNamed(), fNBins(nbins), fMin(min), fMax(max), fBWidth((max-min)/nbins), fBins(NULL){}
    QAxis(const Int_t &nbins, const Double_t* bins): TNamed(), fNBins(0), fMin(0), fMax(0), fBWidth(0), fBins(NULL){Set(nbins,bins);}
    QAxis(const Int_t &nbins, const Float_t* bins): TNamed(), fNBins(0), fMin(0), fMax(0), fBWidth(0), fBins(NULL){Set(nbins,bins);}
    QAxis(const Int_t &nbins, const Int_t* bins): TNamed(), fNBins(0), fMin(0), fMax(0), fBWidth(0), fBins(NULL){Set(nbins,bins);}
    QAxis(const Char_t *name, const Char_t *title): TNamed(name,title), fNBins(1), fMin(0), fMax(1), fBWidth(1), fBins(NULL){}
    QAxis(const Char_t *name, const Char_t *title, const Int_t &nbins, const Double_t& min, const Double_t& max): TNamed(name,title), fNBins(nbins), fMin(min), fMax(max), fBWidth((max-min)/nbins), fBins(NULL){}
    QAxis(const Char_t *name, const Char_t *title, const Int_t &nbins, const Double_t* bins): TNamed(name,title), fNBins(0), fMin(0), fMax(0), fBWidth(0), fBins(NULL){Set(nbins,bins);}
    QAxis(const Char_t *name, const Char_t *title, const Int_t &nbins, const Float_t* bins): TNamed(name,title), fNBins(0), fMin(0), fMax(0), fBWidth(0), fBins(NULL){Set(nbins,bins);}
    QAxis(const Char_t *name, const Char_t *title, const Int_t &nbins, const Int_t* bins): TNamed(name,title), fNBins(0), fMin(0), fMax(0), fBWidth(0), fBins(NULL){Set(nbins,bins);}
    QAxis(const QAxis &rhs): TNamed(rhs), fNBins(rhs.fNBins), fMin(rhs.fMin), fMax(rhs.fMax), fBWidth(rhs.fBWidth), fBins(NULL){if(rhs.fBins) {fBins=new Double_t[fNBins+1]; memcpy(fBins,rhs.fBins,(fNBins+1)*sizeof(Double_t));}}
    virtual ~QAxis(){Clear();}
    void Clear(Option_t* option = ""){if(fBins) {delete[] fBins; fBins=NULL;}}
    TObject* Clone(const char* newname = NULL) const{QAxis *ret=new QAxis(*this); if(newname) ret->SetName(newname); return ret;}

    inline Int_t FindBin(const Double_t &x) const{

      if(x<fMin) return 0;

      else if(x>=fMax) return fNBins+1;

      else if(fBins) {
	return std::upper_bound(fBins, fBins+fNBins+1, x)-fBins;

      } else {
	return 1+int((x-fMin)/fBWidth);
      }
    }

    inline Double_t GetBinCenter(const Int_t &bin) const{
      if(!fBins || bin<1 || bin>fNBins) {
	return fMin+(bin-0.5)*fBWidth;

      } else {
	return (fBins[bin-1]+fBins[bin])*0.5;
      }
    }

    inline Double_t GetBinLowEdge(const Int_t &bin) const{
      if(!fBins || bin<1 || bin>fNBins) {
	return fMin+(bin-1)*fBWidth;

      } else {
	return fBins[bin-1];
      }
    }

    inline Double_t GetBinUpEdge(const Int_t &bin) const{
      if(!fBins || bin<1 || bin>fNBins) {
	return fMin+bin*fBWidth;

      } else {
	return fBins[bin];
      }
    }

    inline Double_t GetBinWidth(const Int_t &bin) const{
      if(!fBins || bin<1 || bin>fNBins) {
	return fBWidth;

      } else {
	return fBins[bin]-fBins[bin-1];
      }
    }

    inline const Double_t& GetMin() const{return fMin;}
    inline const Double_t& GetMax() const{return fMax;}
    inline const Double_t* GetBins() const{return fBins;}
    inline const Int_t& GetNBins() const{return fNBins;}
    const QAxis& operator=(const QAxis &rhs){TNamed::operator=(rhs); Clear(); fNBins=rhs.fNBins; fMin=rhs.fMin; fMax=rhs.fMax; fBWidth=rhs.fBWidth; if(rhs.fBins) {fBins=new Double_t[fNBins+1]; memcpy(fBins,rhs.fBins,(fNBins+1)*sizeof(Double_t));} return *this;}
    void Set(const Int_t &nbins, const Double_t& min, const Double_t& max){Clear(); fNBins=nbins; fMin=min; fMax=max; fBWidth=(fMax-fMin)/nbins;}
    template <typename U> void Set(const Int_t &nbins, const U* bins){if(IsConstantBW(nbins,bins)) Set(nbins,bins[0],bins[nbins]); else {Clear(); fNBins=nbins; fMin=bins[0]; fMax=bins[nbins]; fBWidth=(fMax-fMin)/fNBins; fBins=new Double_t[nbins+1]; for(Int_t i=0; i<=nbins; i++) fBins[i]=bins[i];}}
    void Set(const Int_t &nbins, const Double_t* bins){if(IsConstantBW(nbins,bins)) Set(nbins,bins[0],bins[nbins]); else {Clear(); fNBins=nbins; fMin=bins[0]; fMax=bins[nbins]; fBWidth=(fMax-fMin)/fNBins; fBins=new Double_t[nbins+1]; memcpy(fBins,bins,(nbins+1)*sizeof(Double_t));}}

  protected:
    template <typename U> Bool_t IsConstantBW(const Int_t &nbins, const U *bins) const;

  private:
    Int_t fNBins;
    Double_t fMin;
    Double_t fMax;
    Double_t fBWidth;
    Double_t *fBins;

    ClassDef(QAxis, 1) //Optimized axis class
};

template void QAxis::Set(const Int_t &nbins, const Float_t* bins);
template void QAxis::Set(const Int_t &nbins, const Int_t* bins);
template Bool_t QAxis::IsConstantBW(const Int_t &nbins, const Double_t *bins) const;
template Bool_t QAxis::IsConstantBW(const Int_t &nbins, const Float_t *bins) const;
template Bool_t QAxis::IsConstantBW(const Int_t &nbins, const Int_t *bins) const;

template <typename U> Bool_t QAxis::IsConstantBW(const Int_t &nbins, const U *bins) const
{
  if(!nbins) return kTRUE;
  U bw=bins[1]-bins[0];

  for(Int_t i=nbins; i>=2; --i) {
    if(bins[i]<bins[i-1]) {
      fprintf(stderr,"Error: QAxis::IsConstanBW: Bins must be in increasing order\n");
      throw 1;
    }
    if(bins[i]-bins[i-1]!=bw) return kFALSE;
  }
  return kTRUE;
}

#endif
