#ifndef _QTHN_
#define _QTHN_

#include <cstdio>
#include <cstdlib>
#include "TNamed.h"
#include "TMath.h"
#include "TAxis.h"

template <typename U> class QTHN: public TNamed
{
  public:
    QTHN(): TNamed(),fNDims(0), fAxes(NULL), fNBins(0), fBins(NULL), fEntries(0), fBinContent(NULL), fZero(0), fMaxNBins(0){};
    QTHN(const QTHN &qthn);
    QTHN(const Char_t *name, const Char_t *title, Int_t ndims);
    virtual ~QTHN(){Clear();}
    void AddBinContent(const Long64_t &bin, const U &w=1);
    void AddBinContent(const Int_t *coords, const U &w=1);
    void AddFBinContent(const Long64_t &fbin, const U &w=1);
    void Clear(Option_t* option="");
    TObject* Clone(const char* newname = NULL) const{QTHN<U>* ret=new QTHN(*this); if(newname) ret->SetName(newname); return ret;}
    Int_t Fill(const Double_t *x, const U &w=1);
    Long64_t FindBin(const Double_t *x) const;
    TH1* GenTH(const char *name="_qth2th") const;
    TAxis* GetAxis(Int_t axis) const;
    Long64_t GetBin(const Int_t *coords) const;
    Long64_t GetFBin(const Int_t *coords) const;
    Long64_t GetFBin(const Long64_t &bin) const;
    const Int_t& GetNDims() const {return fNDims;}
    const Long64_t& GetNFbins() const {return fNBins;}
    const U& GetBinContent(const Long64_t &bin) const;
    const Double_t& GetEntries() const{return fEntries;}
    const U& GetFBinContent(const Long64_t &fbin) const;
    void GetBinCoords(Long64_t bin, Int_t *coords) const;
    const Long64_t& GetFBinCoord(const Long64_t &fbin) const;
    void GetFBinCoords(const Long64_t &fbin, Int_t *coords) const;
    Double_t Integral(Int_t** binranges=NULL, Bool_t *widths=NULL) const;
    const QTHN<U>& operator=(const QTHN<U> &qthn);
    QTHN* Projection(const char *name="_pd", const Int_t *axes=NULL, Int_t naxes=0) const;
    void Reset();
    void Scale(const Double_t &scale);
    void ScaleBinContent(const Long64_t &bin, const Double_t &scale);
    void ScaleBinContent(const Int_t *coords, const Double_t &scale);
    void ScaleFBinContent(const Long64_t &fbin, const Double_t &scale);
    void SetAxis(Int_t axis, Int_t nbins, Double_t min, Double_t max);
    void SetAxis(Int_t axis, Int_t nbins, Double_t *bins);
    void SetAxis(Int_t axis, const TAxis* anaxis);
    void SetBinContent(const Long64_t &bin, const U &content);
    void SetBinContent(const Int_t *coords, const U &content);
    void SetFBinContent(const Long64_t &fbin, const U &content);
  protected:
    void ComputeMaxNBins();
    Bool_t IsConstantBW(const Int_t &nbins, const Double_t *bins) const;
    Bool_t IsFBinIncluded(const Long64_t &bin, const Int_t *mins, const Int_t *maxs) const;
  private:
    Int_t fNDims;
    TAxis **fAxes; //!
    Long64_t fNBins;
    Long64_t *fBins; //!
    Double_t fEntries;
    U *fBinContent; //!
    const U fZero;
    Long64_t fMaxNBins;

    ClassDef(QTHN,1) //Multidimensional histogram template class
};

#include "QTHN_cxx.h"

#endif
