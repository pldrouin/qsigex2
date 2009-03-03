// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QHNF_
#define _QHNF_

#include <cstdio>
#include <cstdlib>
#include "TMath.h"
#include "QAxis.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "QHN.h"

template <typename U> class QHNF: public QHN<U>
{
  public:
    QHNF(): QHN<U>(), fZero(0), fNFBins(0), fBins(NULL){};
    QHNF(const QHNF &qthn);
    QHNF(const QHN<U> &qthn);
    QHNF(const Char_t *name, const Char_t *title, Int_t ndims): QHN<U>(name,title,ndims), fZero(0), fNFBins(0), fBins(NULL){}
#ifndef __CINT__
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins,kFALSE), fZero(0){Init();}
#endif

    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
#ifndef __CINT__
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
#endif
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
#ifndef __CINT__
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins,kFALSE), fZero(0){Init();}
#endif
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh,kFALSE), fZero(0){Init();}
    QHNF(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins,kFALSE), fZero(0){Init();}

    virtual ~QHNF(){Clear();}
    virtual void AddBinContent(const Long64_t &bin, const U &w=1);
    void AddFBinContent(const Long64_t &fbin, const U &w=1){QHN<U>::fBinContent[fbin]+=w; QHN<U>::fEntries+=w;}
    virtual void Clear(Option_t* option="");
    TObject* Clone(const char* newname = NULL) const{QHNF<U>* ret=new QHNF(*this); if(newname) ret->SetName(newname); return ret;}
    virtual Long64_t GetFBin(const Int_t *coords) const;
    virtual Long64_t GetFBin(const Long64_t &bin) const;
    const Long64_t& GetNFbins() const {return fNFBins;}
    virtual const U& GetBinContent(const Long64_t &bin) const;
    const U& GetFBinContent(const Long64_t &fbin) const{return QHN<U>::fBinContent[fbin];}
    const Long64_t& GetFBinCoord(const Long64_t &fbin) const{return fBins[fbin];}
    void GetFBinCoords(const Long64_t &fbin, Int_t *coords) const;
    virtual QHN<U>* New() const{return new QHNF<U>;}
    virtual QHN<U>* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHNF<U>(name,title,ndims);}
    const QHNF<U>& operator=(const QHNF<U> &qthn);
    const QHNF<U>& operator=(const QHN<U> &qthn);
    virtual void Reset();
    void ScaleBinContent(const Long64_t &bin, const Double_t &scale);
    void ScaleBinContent(const Int_t *coords, const Double_t &scale);
    virtual void SetBinContent(const Long64_t &bin, const U &content);
    void SetBinContent(const Int_t *coords, const U &content);
    void SetFBinContent(const Long64_t &fbin, const U &content);
  protected:
    virtual QHN<Double_t>* NewD() const{return new QHNF<Double_t>;}
    virtual QHN<Double_t>* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHNF<Double_t>(name,title,ndims);}
    virtual void ComputeNBins();
    void Init();
    Bool_t IsFBinIncluded(const Long64_t &bin, const Int_t *mins, const Int_t *maxs) const;
    const U fZero;
    Long64_t fNFBins;
    Long64_t *fBins; //!

    ClassDef(QHNF,1) //Multidimensional histogram template class optimized for memory and iteration over filled bins
};

typedef QHNF<Double_t> QHNF_D;
typedef QHNF<Float_t> QHNF_F;
typedef QHNF<Int_t> QHNF_I;

#include "QHNF_cxx.h"

#endif
