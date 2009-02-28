// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QTHNF_
#define _QTHNF_

#include <cstdio>
#include <cstdlib>
#include "TMath.h"
#include "QAxis.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "QTHN.h"

template <typename U> class QTHNF: public QTHN<U>
{
  public:
    QTHNF(): QTHN<U>(), fNFBins(0), fBins(NULL), fZero(0){};
    QTHNF(const QTHNF &qthn);
    QTHNF(const QTHN<U> &qthn);
    QTHNF(const Char_t *name, const Char_t *title, Int_t ndims): QTHN<U>(name,title,ndims), fNFBins(0), fBins(NULL), fZero(0){}
    virtual ~QTHNF(){Clear();}
    virtual void AddBinContent(const Long64_t &bin, const U &w=1);
    void AddBinContent(const Int_t *coords, const U &w=1){AddBinContent(QTHN<U>::GetBin(coords),w);}
    void AddFBinContent(const Long64_t &fbin, const U &w=1){QTHN<U>::fBinContent[fbin]+=w; QTHN<U>::fEntries++;}
    virtual void Clear(Option_t* option="");
    TObject* Clone(const char* newname = NULL) const{QTHNF<U>* ret=new QTHNF(*this); if(newname) ret->SetName(newname); return ret;}
    virtual Long64_t GetFBin(const Int_t *coords) const;
    virtual Long64_t GetFBin(const Long64_t &bin) const;
    const Long64_t& GetNFbins() const {return fNFBins;}
    virtual const U& GetBinContent(const Long64_t &bin) const;
    const U& GetFBinContent(const Long64_t &fbin) const{return QTHN<U>::fBinContent[fbin];}
    const Long64_t& GetFBinCoord(const Long64_t &fbin) const{return fBins[fbin];}
    void GetFBinCoords(const Long64_t &fbin, Int_t *coords) const;
    const QTHNF<U>& operator=(const QTHNF<U> &qthn);
    const QTHNF<U>& operator=(const QTHN<U> &qthn);
    virtual QTHN<U>* Projection(const char *name="_pd", const Int_t *axes=NULL, const Int_t &naxes=0, QTHN<U> *th=NULL) const;
    virtual void Reset();
    void ScaleBinContent(const Long64_t &bin, const Double_t &scale);
    void ScaleBinContent(const Int_t *coords, const Double_t &scale);
    virtual void SetBinContent(const Long64_t &bin, const U &content);
    void SetBinContent(const Int_t *coords, const U &content);
    void SetFBinContent(const Long64_t &fbin, const U &content);
  protected:
    virtual void ComputeNBins();
    Bool_t IsFBinIncluded(const Long64_t &bin, const Int_t *mins, const Int_t *maxs) const;
    Long64_t fNFBins;
    Long64_t *fBins; //!
    const U fZero;

    ClassDef(QTHNF,1) //Multidimensional histogram template class optimized for memory and iteration over filled bins
};

#include "QTHNF_cxx.h"

#endif
