// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QTHN_
#define _QTHN_

#include <cstdio>
#include <cstdlib>
#include "TNamed.h"
#include "TMath.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "TMatrixDSym.h"
#include "QAxis.h"

template <typename U> class QTHN: public TNamed
{
  public:
    QTHN(): TNamed(),fNDims(0), fAxes(NULL), fEntries(0), fBinContent(NULL), fNBins(0){};
    QTHN(const QTHN &qthn);
    QTHN(const Char_t *name, const Char_t *title, const Int_t &ndims);
    virtual ~QTHN(){Clear();}
    virtual void AddBinContent(const Long64_t &bin, const U &w=1){fBinContent[bin]+=w; fEntries++;}
    void AddBinContent(const Int_t *coords, const U &w=1){AddBinContent(GetBin(coords),w);}
    virtual void AddFBinContent(const Long64_t &fbin, const U &w=1){AddBinContent(fbin,w);}
    virtual void Clear(Option_t* option="");
    virtual TObject* Clone(const char* newname = NULL) const{QTHN<U>* ret=new QTHN(*this); if(newname) ret->SetName(newname); return ret;}
    void Draw(Option_t *option=""){if(fNDims<4) GenTH()->Draw(option);}
    void Fill(Double_t const * const &x, const U &w=1){AddBinContent(FindBin(x));}
    template <typename V> Long64_t FindBin(const V &x0) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2, const V &x3) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6, const V &x7) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6, const V &x7, const V &x8) const;
    template <typename V> Long64_t FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6, const V &x7, const V &x8, const V &x9) const;
    Long64_t FindBin(Double_t const* const &x) const;
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
    Double_t Integral(Int_t const* const* binranges=NULL, const Bool_t *widths=NULL) const;
    virtual const QTHN<U>& operator=(const QTHN<U> &qthn);
    virtual QTHN<U>* Projection(const char *name="_pd", const Int_t *axes=NULL, const Int_t &naxes=0, QTHN<U> *th=NULL) const;
    virtual void Reset();
    void Scale(const Double_t &scale){for(Long64_t li=GetNFbins(); li>=0; --li) fBinContent[li]*=(U)scale;}
    virtual void ScaleBinContent(const Long64_t &bin, const Double_t &scale);
    virtual void ScaleBinContent(const Int_t *coords, const Double_t &scale);
    void ScaleFBinContent(const Long64_t &fbin, const Double_t &scale){fBinContent[fbin]*=(U)scale;}
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t &min, const Double_t &max);
    void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t *bins);
    void SetAxis(const Int_t &axis, const QAxis* anaxis);
    virtual void SetBinContent(const Long64_t &bin, const U &content){fBinContent[bin]=content;}
    void SetBinContent(const Int_t *coords, const U &content){SetBinContent(GetBin(coords),content);}
    void SetEntries(Double_t n){fEntries=n;}
    void SetNDims(const Int_t &ndims){Clear(); fAxes=new QAxis*[ndims]; fNDims=ndims;}
    virtual void SetFBinContent(const Long64_t &fbin, const U &content){SetBinContent(fbin, content);}
  protected:
    virtual void ComputeNBins();
    virtual Bool_t IsFBinIncluded(const Long64_t &bin, const Int_t *mins, const Int_t *maxs) const;
    Int_t fNDims;
    QAxis **fAxes; //!
    Double_t fEntries;
    U *fBinContent; //!
    Long64_t fNBins;

    ClassDef(QTHN,1) //Multidimensional histogram template class optimized for random access
};

#include "QTHN_cxx.h"

#endif
