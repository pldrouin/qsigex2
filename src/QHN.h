// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QHN_
#define _QHN_

#include <cstdio>
#include <cstdlib>
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "TMath.h"
#include "TMatrixDSym.h"
#include "QDis.h"
#include "QAxis.h"

template <typename U> class QHN: public QDis
{
  public:
    QHN(): QDis(),fNDims(0), fAxes(NULL), fEntries(0), fBinContent(NULL), fNBins(0){};
    QHN(const QHN &qthn);
    QHN(const Char_t *name, const Char_t *title, const Int_t &ndims);
#ifndef __CINT__
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins);
#endif

    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0);
#ifndef __CINT__
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0);
#endif
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0);
#ifndef __CINT__
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins);
#endif
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0);
    QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins);

    virtual ~QHN(){Clear();}
    virtual void AddBinContent(const Long64_t &bin, const U &w=1){fBinContent[bin]+=w; fEntries+=w;}
    void AddBinContent(const Int_t *coords, const U &w=1){AddBinContent(GetBin(coords),w);}
    virtual void AddFBinContent(const Long64_t &fbin, const U &w=1){AddBinContent(fbin,w);}
    virtual void Clear(Option_t* option="");
    virtual TObject* Clone(const char* newname = NULL) const{QHN<U>* ret=new QHN(*this); if(newname) ret->SetName(newname); return ret;}
    QDis* CloneQDis() const{return new QHN<U>(*this);}
    void Draw(Option_t *option=""){if(fNDims<4) GenTH()->Draw(option);}
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
    void Fill(Double_t const * const &x, const U &w=1){AddBinContent(FindBin(x));}
    void Fill(Float_t const * const &x, const U &w=1){AddBinContent(FindBin(x));}
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
    TH1* GenTH(const char *name="_th") const;
    QAxis* GetAxis(const Int_t &axis) const{return fAxes[axis];}
    Long64_t GetBin(const Int_t *coords) const;
    const U& GetBinContent(const Int_t *coords) const{return GetBinContent(GetBin(coords));}
    virtual const U& GetBinContent(const Long64_t &bin) const{return fBinContent[bin];}
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
    QHN<U>* Projection(const char *name="_pd", const Int_t *axes=NULL, const Int_t &naxes=0) const;
    virtual void Reset();
    void Scale(const Double_t &scale){for(Long64_t li=GetNFbins(); li>=0; --li) fBinContent[li]*=(U)scale;}
    virtual void ScaleBinContent(const Long64_t &bin, const Double_t &scale);
    virtual void ScaleBinContent(const Int_t *coords, const Double_t &scale);
    void ScaleFBinContent(const Long64_t &fbin, const Double_t &scale){fBinContent[fbin]*=(U)scale;}
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t &min, const Double_t &max);
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t *bins);
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Float_t *bins);
    void SetAxis(const Int_t &axis, const QAxis* anaxis);
    virtual void SetBinContent(const Long64_t &bin, const U &content){fBinContent[bin]=content;}
    void SetBinContent(const Int_t *coords, const U &content){SetBinContent(GetBin(coords),content);}
    void SetEntries(Double_t n){fEntries=n;}
    void SetNDims(const Int_t &ndims){Clear(); fAxes=new QAxis*[ndims]; fNDims=ndims;}
    virtual void SetFBinContent(const Long64_t &fbin, const U &content){SetBinContent(fbin, content);}
    void TerminateProcObj(){Normalize();}
  protected:
    virtual QHN<Double_t>* NewD() const{return new QHN<Double_t>;}
    virtual QHN<Double_t>* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHN<Double_t>(name,title,ndims);}
    virtual void ComputeNBins();
    virtual Bool_t IsFBinIncluded(const Long64_t &bin, const Int_t *mins, const Int_t *maxs) const;
    Int_t fNDims;
    QAxis **fAxes; //!
    Double_t fEntries;
    U *fBinContent; //!
    Long64_t fNBins;

    ClassDef(QHN,1) //Multidimensional histogram template class optimized for random access
};

#include "QHN_cxx.h"

#endif