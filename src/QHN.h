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
    QHN(): QDis(),fNDims(0), fAxes(NULL), fEntries(0), fBinContent(NULL), fNBins(0), fIntUseFBinLoop(kFALSE), fTH(NULL){};
    QHN(const QHN &qthn);
    QHN(const Char_t* filename, const Char_t* objectname);
    QHN(const Char_t *name, const Char_t *title, const Int_t &ndims);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init=kTRUE);

    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0, const Bool_t &init=kTRUE);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init=kTRUE);

    virtual ~QHN(){Clear();}
    void Add(const QHN<U> *qhn, const U &c=1);
    inline virtual void AddBinContent(const Long64_t &bin, const U &w=1){
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QHN::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      fBinContent[bin]+=w; fEntries+=w;
    }
    inline void AddBinContent(const Int_t *coords, const U &w=1){AddBinContent(GetBin(coords),w);}
    const Double_t& AddEntries(const Double_t &nentries=1){return (fEntries+=nentries);}
    inline virtual void AddFBinContent(const Long64_t &fbin, const U &w=1){AddBinContent(fbin,w);}
    virtual void Clear(Option_t* option="");
    virtual TObject* Clone(const char* newname = "") const{QHN<U>* ret=new QHN<U>(*this); if(strcmp(newname,"")) dynamic_cast<TNamed*>(ret)->SetName(newname); return dynamic_cast<TObject*>(ret);}
    QDis* CloneQDis() const{return new QHN<U>(*this);}
    void Divide(const QHN<U> *qhn);
    void Draw(Option_t *option=""){if(fNDims<4) const_cast<TH1&>(GetTH()).Draw(option);}
    inline const U& Eval(const Double_t &x0) const{return GetBinContent(FindBin(x0));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1) const{return GetBinContent(FindBin(x0,x1));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2) const{return GetBinContent(FindBin(x0,x1,x2));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3) const{return GetBinContent(FindBin(x0,x1,x2,x3));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    inline const U& Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &x9) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
    inline const U& Eval(Double_t const* const &x) const{return GetBinContent(FindBin(x));}
    inline const U& Eval(const Float_t &x0) const{return GetBinContent(FindBin(x0));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1) const{return GetBinContent(FindBin(x0,x1));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2) const{return GetBinContent(FindBin(x0,x1,x2));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3) const{return GetBinContent(FindBin(x0,x1,x2,x3));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    inline const U& Eval(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &x9) const{return GetBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
    inline const U& Eval(Float_t const* const &x) const{return GetBinContent(FindBin(x));}
    inline virtual void Fill(Double_t const * const &x, const U &w=1){AddBinContent(FindBin(x),w);}
    inline virtual void Fill(const Double_t &x0){AddBinContent(FindBin(x0));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1){AddBinContent(FindBin(x0,x1));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2){AddBinContent(FindBin(x0,x1,x2));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3){AddBinContent(FindBin(x0,x1,x2,x3));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4){AddBinContent(FindBin(x0,x1,x2,x3,x4));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    inline virtual void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &x9){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
    inline virtual void Fill(Float_t const * const &x, const U &w=1){AddBinContent(FindBin(x),w);}
    inline virtual void Fill(const Float_t &x0){AddBinContent(FindBin(x0));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1){AddBinContent(FindBin(x0,x1));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2){AddBinContent(FindBin(x0,x1,x2));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3){AddBinContent(FindBin(x0,x1,x2,x3));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4){AddBinContent(FindBin(x0,x1,x2,x3,x4));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8));}
    inline virtual void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &x9){AddBinContent(FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9));}
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
    QAxis* const& GetAxis(const Int_t &axis) const{return fAxes[axis];}
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
    inline const U& GetBinContent(const Int_t *coords) const{return GetBinContent(GetBin(coords));}
    inline virtual const U& GetBinContent(const Long64_t &bin) const{return fBinContent[bin];}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1) const{return GetBinContent(GetBin(coord0,coord1));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2) const{return GetBinContent(GetBin(coord0,coord1,coord2));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6,coord7));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6,coord7,coord8));}
    inline const U& GetBinContent(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8, const Int_t &coord9) const{return GetBinContent(GetBin(coord0,coord1,coord2,coord3,coord4,coord5,coord6,coord7,coord8,coord9));}
    void GetCorrelationMatrix(TMatrixDSym* covmat, Bool_t width=kTRUE, Bool_t varianceondiag=kFALSE) const;
    void GetCovarianceMatrix(TMatrixDSym* covmat, Bool_t width=kTRUE) const;
    inline virtual Long64_t GetFBin(const Int_t *coords) const{return GetBin(coords);}
    inline virtual Long64_t GetFBin(const Long64_t &bin) const{return bin;}
    inline const Int_t& GetNDims() const {return fNDims;}
    inline virtual const Long64_t& GetNFbins() const {return fNBins;}
    inline const Long64_t& GetNBins() const {return fNBins;}
    inline const Double_t& GetEntries() const{return fEntries;}
    inline virtual const U& GetFBinContent(const Long64_t &fbin) const{return fBinContent[fbin];}
    void GetBinCoords(Long64_t bin, Int_t *coords) const;
    inline virtual const Long64_t& GetFBinCoord(const Long64_t &fbin) const{return fbin;}
    inline virtual void GetFBinCoords(const Long64_t &fbin, Int_t *coords) const{return GetBinCoords(fbin,coords);}
    const Bool_t& GetIntUseFBinLoop() const {return fIntUseFBinLoop;}
    virtual void CopyStruct(const QHN<U> &qthn);
    void GetMeans(Double_t means[], Bool_t width=kTRUE) const;
    inline void InitProcObj(){Reset();}
    Double_t Integral(Int_t const* const* binranges=NULL, const Bool_t *widths=NULL) const;
    QHN<Double_t>* MarginalPDF(const char *name="_md", const Int_t *axes=NULL, const Int_t &naxes=0) const;
    QHN<Double_t>* MarginalPDF(const char *name, const Int_t &axis0) const{return MarginalPDF(name,&axis0,1);}
    QHN<Double_t>* MarginalPDF(const char *name, const Int_t &axis0, const Int_t &axis1) const;
    QHN<Double_t>* MarginalPDF(const char *name, const Int_t &axis0, const Int_t &axis1, const Int_t &axis2) const;
    void Multiply(const QHN<U> *qhn);
    virtual QHN<U>* New() const{return new QHN<U>;}
    virtual QHN<U>* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHN<U>(name,title,ndims);}
    void Normalize(Double_t* integral=NULL, Bool_t reverse=kFALSE);
    virtual const QHN<U>& operator=(const QHN<U> &qthn);
    const QHN<U>& operator=(const TH1 &th);
    const QHN<U>& operator=(const QDisTH &qth);
    QHN<U>* Projection(const char *name="_pd", const Int_t *axes=NULL, const Int_t &naxes=0) const;
    QHN<U>* Projection(const char *name, const Int_t &axis0) const{return Projection(name,&axis0,1);}
    QHN<U>* Projection(const char *name, const Int_t &axis0, const Int_t &axis1) const;
    QHN<U>* Projection(const char *name, const Int_t &axis0, const Int_t &axis1, const Int_t &axis2) const;
    virtual void Reset();
    inline void Scale(const Double_t &scale){for(Long64_t li=GetNFbins()-1; li>=0; --li) fBinContent[li]*=(U)scale;}
    virtual void ScaleBinContent(const Long64_t &bin, const Double_t &scale);
    virtual void ScaleBinContent(const Int_t *coords, const Double_t &scale);
    inline void ScaleFBinContent(const Long64_t &fbin, const Double_t &scale){
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
    inline virtual void SetBinContent(const Long64_t &bin, const U &content){
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QHN::SetBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      fBinContent[bin]=content;
    }
    inline void SetBinContent(const Int_t *coords, const U &content){SetBinContent(GetBin(coords),content);}
    inline void SetEntries(Double_t n){fEntries=n;}
    void SetNDims(const Int_t &ndims){Clear(); fAxes=new QAxis*[ndims]; fNDims=ndims;}
    inline virtual void SetFBinContent(const Long64_t &fbin, const U &content){SetBinContent(fbin, content);}
    QHN<U>* SubHist(const char *name="_pd", const Int_t *axes=NULL, const Int_t *firstbins=NULL, const Int_t *lastbins=NULL, const Int_t &naxes=0) const;
    QHN<U>* SubHist(const char *name, const Int_t &axis0, const Int_t &firstbin0, const Int_t &lastbin0) const{return SubHist(name,&axis0,&firstbin0,&lastbin0,1);}
    QHN<U>* SubHist(const char *name, const Int_t &axis0, const Int_t &firstbin0, const Int_t &lastbin0, const Int_t &axis1, const Int_t &firstbin1, const Int_t &lastbin1) const;
    QHN<U>* SubHist(const char *name, const Int_t &axis0, const Int_t &firstbin0, const Int_t &lastbin0, const Int_t &axis1, const Int_t &firstbin1, const Int_t &lastbin1, const Int_t &axis2, const Int_t &firstbin2, const Int_t &lastbin2) const;
    void SetIntUseFBinLoop(const Bool_t &intusefbinloop){fIntUseFBinLoop=intusefbinloop;}
    inline void TerminateProcObj(){Normalize();}
    void WritePSPlot(const char *filename=NULL) const;
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
    Bool_t fIntUseFBinLoop;
    TH1 *fTH;

    ClassDef(QHN,2) //Multidimensional histogram template class optimized for random access
};

typedef QHN<Double_t> QHN_D;
typedef QHN<Float_t> QHN_F;
typedef QHN<Int_t> QHN_I;

#include "QHN_cxx.h"

#endif
