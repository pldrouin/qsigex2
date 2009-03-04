// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QHN_
#define _QHN_

#include <cstdio>
#include <cstdlib>
#include "TFile.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "TMath.h"
#include "TMatrixDSym.h"
#include "QDisTH.h"
#include "QAxis.h"

template <typename U> class QHN: public QDis
{
  public:
    QHN(): QDis(),fNDims(0), fAxes(NULL), fEntries(0), fBinContent(NULL), fNBins(0), fTH(NULL){};
    QHN(const QHN &qthn);
#ifndef __CINT__
    QHN(const Char_t* filename, const Char_t* objectname);
#endif
    QHN(const Char_t *name, const Char_t *title, const Int_t &ndims);
#ifndef __CINT__
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);
#endif

    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
#ifndef __CINT__
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
#endif
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
#ifndef __CINT__
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);
#endif
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);

    virtual ~QHN(){Clear();}
    virtual void AddBinContent(const Long64_t &bin, const U &w=1){
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QHN::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      fBinContent[bin]+=w; fEntries+=w;
    }
    void AddBinContent(const Int_t *coords, const U &w=1){AddBinContent(GetBin(coords),w);}
    virtual void AddFBinContent(const Long64_t &fbin, const U &w=1){AddBinContent(fbin,w);}
    virtual void Clear(Option_t* option="");
    virtual TObject* Clone(const char* newname = NULL) const{QHN<U>* ret=new QHN(*this); if(newname) ret->SetName(newname); return ret;}
    QDis* CloneQDis() const{return new QHN<U>(*this);}
    void Draw(Option_t *option=""){if(fNDims<4) const_cast<TH1&>(GetTH()).Draw(option);}
    const U& Eval(const Double_t &x0) const{return GetBinContent(FindBin(x0));}
    const U& Eval(const Double_t &x0, const Double_t &x1) const{return GetBinContent(FindBin(x0,x1));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2) const{return GetBinContent(FindBin(x0,x1,x2));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3) const{return GetBinContent(FindBin(x0,x1,x2,x3));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &x9) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
    const U& Eval(Double_t const* const &x) const{return GetBinContent(FindBin(x));}
    const U& Eval(const Float_t &x0) const{return GetBinContent(FindBin(x0));}
    const U& Eval(const Float_t &x0, const Float_t &x1) const{return GetBinContent(FindBin(x0,x1));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2) const{return GetBinContent(FindBin(x0,x1,x2));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3) const{return GetBinContent(FindBin(x0,x1,x2,x3));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &x9) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
    const U& Eval(Float_t const* const &x) const{return GetBinContent(FindBin(x));}
    virtual void Fill(Double_t const * const &x, const U &w=1){AddBinContent(FindBin(x),w);}
    virtual void Fill(const Double_t &x0){AddBinContent(FindBin(x0));}
    virtual void Fill(const Double_t &x0, const Double_t &x1){AddBinContent(FindBin(x0,x1));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2){AddBinContent(FindBin(x0,x1,x2));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3){AddBinContent(FindBin(x0,x1,x2,x3));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4){AddBinContent(FindBin(x0,x1,x2,x3,x4));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &x9){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
    virtual void Fill(Float_t const * const &x, const U &w=1){AddBinContent(FindBin(x),w);}
    virtual void Fill(const Float_t &x0){AddBinContent(FindBin(x0));}
    virtual void Fill(const Float_t &x0, const Float_t &x1){AddBinContent(FindBin(x0,x1));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2){AddBinContent(FindBin(x0,x1,x2));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3){AddBinContent(FindBin(x0,x1,x2,x3));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4){AddBinContent(FindBin(x0,x1,x2,x3,x4));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &x9){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
    Long64_t FindBin(const Double_t &x0) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8) const;
    Long64_t FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &x9) const;
    Long64_t FindBin(Double_t const* const &x) const;
    Long64_t FindBin(const Float_t &x0) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8) const;
    Long64_t FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &x9) const;
    Long64_t FindBin(Float_t const* const &x) const;
    const TH1& GetTH();
    QAxis* GetAxis(const Int_t &axis) const{return fAxes[axis];}
    Long64_t GetBin(const Int_t *coords) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8) const;
    Long64_t GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8, const Int_t &coord9) const;
    const U& GetBinContent(const Int_t *coords) const{return GetBinContent(GetBin(coords));}
    virtual const U& GetBinContent(const Long64_t &bin) const{return fBinContent[bin];}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1) const{return GetBinContent(GetBin(coord0,coord1));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2) const{return GetBinContent(GetBin(coord0,coord1,coord2));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6,coord7));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6,coord7,coord8));}
    const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8, const Int_t &coord9) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6,coord7,coord8,coord9));}
    void GetCorrelationMatrix(TMatrixDSym* covmat, Bool_t width=kTRUE, Bool_t varianceondiag=kFALSE) const;
    void GetCovarianceMatrix(TMatrixDSym* covmat, Bool_t width=kTRUE) const;
    virtual Long64_t GetFBin(const Int_t *coords) const{return GetBin(coords);}
    virtual Long64_t GetFBin(const Long64_t &bin) const{return bin;}
    const Int_t& GetNDims() const {return fNDims;}
    virtual const Long64_t& GetNFbins() const {return fNBins;}
    const Long64_t& GetNBins() const {return fNBins;}
    const Double_t& GetEntries() const{return fEntries;}
    virtual const U& GetFBinContent(const Long64_t &fbin) const{return fBinContent[fbin];}
    void GetBinCoords(Long64_t bin, Int_t *coords) const;
    virtual const Long64_t& GetFBinCoord(const Long64_t &fbin) const{return fbin;}
    virtual void GetFBinCoords(const Long64_t &fbin, Int_t *coords) const{return GetBinCoords(fbin,coords);}
    void GetMeans(Double_t means[], Bool_t width=kTRUE) const;
    void InitProcObj(){Reset();}
    Double_t Integral(Int_t const* const* binranges=NULL, const Bool_t *widths=NULL) const;
    QHN<Double_t>* MarginalPDF(const char *name="_md", const Int_t *axes=NULL, const Int_t &naxes=0) const;
    virtual QHN<U>* New() const{return new QHN<U>;}
    virtual QHN<U>* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHN<U>(name,title,ndims);}
    void Normalize(Double_t* integral=NULL);
    virtual const QHN<U>& operator=(const QHN<U> &qthn);
    const QHN<U>& operator=(const TH1 &th);
#ifndef __CINT__
    const QHN<U>& operator=(const QDisTH &qth);
#endif
    QHN<U>* Projection(const char *name="_pd", const Int_t *axes=NULL, const Int_t &naxes=0) const;
    virtual void Reset();
    void Scale(const Double_t &scale){for(Long64_t li=GetNFbins()-1; li>=0; --li) fBinContent[li]*=(U)scale;}
    virtual void ScaleBinContent(const Long64_t &bin, const Double_t &scale);
    virtual void ScaleBinContent(const Int_t *coords, const Double_t &scale);
    void ScaleFBinContent(const Long64_t &fbin, const Double_t &scale){
#ifndef QSFAST
      if(fbin<0 || fbin>=fNBins) {
	fprintf(stderr,"Error: QHN::ScaleFBinContent: %lli is not a valid bin number\n",fbin);
	throw 1;
      }
#endif
      fBinContent[fbin]*=(U)scale;
    }
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t &min, const Double_t &max);
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t *bins);
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Float_t *bins);
    void SetAxis(const Int_t &axis, const QAxis* anaxis);
    virtual void SetBinContent(const Long64_t &bin, const U &content){
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QHN::SetBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      fBinContent[bin]=content;
    }
    void SetBinContent(const Int_t *coords, const U &content){SetBinContent(GetBin(coords),content);}
    void SetEntries(Double_t n){fEntries=n;}
    void SetNDims(const Int_t &ndims){Clear(); fAxes=new QAxis*[ndims]; fNDims=ndims;}
    virtual void SetFBinContent(const Long64_t &fbin, const U &content){SetBinContent(fbin, content);}
    void TerminateProcObj(){Normalize();}
  protected:
    virtual QHN<Double_t>* NewD() const{return new QHN<Double_t>;}
    virtual QHN<Double_t>* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHN<Double_t>(name,title,ndims);}
    virtual void ComputeNBins();
    void Init();
    virtual Bool_t IsFBinIncluded(const Long64_t &bin, const Int_t *mins, const Int_t *maxs) const;
    Int_t fNDims;
    QAxis **fAxes; //!
    Double_t fEntries;
    U *fBinContent; //!
    Long64_t fNBins;
    TH1 *fTH;

    ClassDef(QHN,1) //Multidimensional histogram template class optimized for random access
};

typedef QHN<Double_t> QHN_D;
typedef QHN<Float_t> QHN_F;
typedef QHN<Int_t> QHN_I;

#include "QHN_cxx.h"

#endif
