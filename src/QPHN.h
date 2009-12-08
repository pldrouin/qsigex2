// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QPHN_
#define _QPHN_

#include <cstdio>
#include <cstdlib>
#include "TMath.h"
#include "QAxis.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "QHN.h"

class QPHN: public QHN_D
{
  public:
    QPHN(): QHN_D(), fBinEntries(NULL){};
    QPHN(const QPHN &qthn): QHN_D(qthn){fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t)); memcpy(fBinEntries,qthn.fBinEntries,fNBins*sizeof(Double_t));}
    QPHN(const Char_t *name, const Char_t *title, Int_t ndims): QHN_D(name,title,ndims), fBinEntries(NULL){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){Init();}

    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){Init();}

    virtual ~QPHN(){}
    inline void AddBinContent(const Long64_t &bin, const Double_t &y)
    {
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QPHN::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      QHN_D::fBinContent[bin]+=y; ++(fBinEntries[bin]); ++QHN_D::fEntries;
    }
    inline void AddBinContent(const Long64_t &bin, const Double_t &y, const Double_t &w)
    {
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QPHN::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      QHN_D::fBinContent[bin]+=w*y; fBinEntries[bin]+=w; QHN_D::fEntries+=w;
    }
    void CopyStruct(const QHN_D &qthn);
    void Clear(Option_t* option=""){QHN_D::Clear(); if(fBinEntries) {free(fBinEntries); fBinEntries=NULL;}}
    TObject* Clone(const char* newname = NULL) const{QPHN* ret=new QPHN(*this); if(newname) ret->SetName(newname); return ret;}
    inline void Fill(Double_t const * const &x, const Double_t &y){AddBinContent(FindBin(x),y);}
    inline void Fill(Double_t const * const &x, const Double_t &y, const Double_t &w){AddBinContent(FindBin(x),y,w);}
    inline void Fill(const Double_t &x0, const Double_t &y){AddBinContent(FindBin(x0),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &y){AddBinContent(FindBin(x0,x1),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &y){AddBinContent(FindBin(x0,x1,x2),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &y){AddBinContent(FindBin(x0,x1,x2,x3),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8),y);}
    inline void Fill(Float_t const * const &x, const Double_t &y){AddBinContent(FindBin(x),y);}
    inline void Fill(Float_t const * const &x, const Double_t &y, const Double_t &w){AddBinContent(FindBin(x),y,w);}
    inline void Fill(const Float_t &x0, const Float_t &y){AddBinContent(FindBin(x0),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &y){AddBinContent(FindBin(x0,x1),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &y){AddBinContent(FindBin(x0,x1,x2),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &y){AddBinContent(FindBin(x0,x1,x2,x3),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &y){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8),y);}
    inline const Double_t& GetBinContent(const Long64_t &bin) const{return lBCret=(fBinEntries[bin]?fBinContent[bin]/fBinEntries[bin]:0);}
    inline const Double_t& GetFBinContent(const Long64_t &fbin) const{return lBCret=(fBinEntries[fbin]?fBinContent[fbin]/fBinEntries[fbin]:0);}
    QHN_D* New() const{return new QPHN;}
    QHN_D* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QPHN(name,title,ndims);}
    const QPHN& operator=(const QPHN &qthn);
    inline void Reset(){QHN_D::Reset(); memset(fBinEntries,0,fNBins*sizeof(Double_t));}
  protected:
    QHN_D* NewD() const{return New();}
    QHN_D* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return New(name,title,ndims);}
    void ComputeNBins(){QHN_D::ComputeNBins(); if(fBinEntries) free(fBinEntries); fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t));}
    void Init(){fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t)); memset(fBinEntries,0,fNBins*sizeof(Double_t));}
  private:
    Double_t *fBinEntries;
    mutable Double_t lBCret;
    void AddFBinContent(const Long64_t &fbin, const Double_t &w){}
    void Fill(const Double_t &x0){}
    void Fill(const Float_t &x0){}
    const Bool_t& IsReadThreadSafe() const{return kFALSE;}
    const QPHN& operator=(const QHN_D &qthn){return *this;}
    void ScaleBinContent(const Long64_t &bin, const Double_t &scale){}
    void ScaleBinContent(const Int_t *coords, const Double_t &scale){}
    void SetBinContent(const Long64_t &bin, const Double_t &content){}
    void SetBinContent(const Int_t *coords, const Double_t &content){}

    ClassDef(QPHN,1) //Multidimensional profile histogram class optimized for random access
};

#endif
