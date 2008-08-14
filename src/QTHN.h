#ifndef _QTHN_
#define _QTHN_

#include <cstdio>
#include <cstdlib>
#include "TNamed.h"
#include "TAxis.h"

template <typename U> class QTHN: public TNamed
{
  public:
    QTHN(): TNamed(),fNDims(0), fAxes(NULL), fNBins(0), fBins(NULL), fBinContent(NULL), fZero(0){};
    QTHN(const Char_t *name, const Char_t *title, Int_t ndims);
    virtual ~QTHN();
    void AddBinContent(const Long64_t &bin, const U &w=1);
    Int_t Fill(const Double_t *x, const U &w=1);
    Long64_t FindBin(const Double_t *x) const;
    TAxis* GetAxis(Int_t axis) const;
    Long64_t GetBin(const Int_t *coords) const;
    Int_t GetNDims() const {return fNDims;}
    const Long64_t& GetNFbins() const {return fNBins;}
    const U& GetBinContent(const Long64_t &bin) const;
    const U& GetFBinContent(const Long64_t &fbin) const;
    void GetBinCoords(Long64_t bin, Int_t *coords) const;
    const Long64_t& GetFBinCoord(const Long64_t &fbin) const;
    void GetFBinCoords(const Long64_t &fbin, Int_t *coords) const;
    TH1D* Projection1D(const char *name="_p1d", Int_t xaxis=0) const;
    TH2D* Projection2D(const char *name="_p2d", Int_t xaxis=0, Int_t yaxis=1) const;
    TH3D* Projection3D(const char *name="_p3d", Int_t xaxis=0, Int_t yaxis=1, Int_t zaxis=2) const;
    void Reset();
    void SetAxis(Int_t axis, Int_t nbins, Double_t min, Double_t max);
    void SetAxis(Int_t axis, Int_t nbins, Double_t *bins);
  protected:
  private:
    Int_t fNDims;
    TAxis **fAxes; //!
    Long64_t fNBins;
    Long64_t *fBins; //!
    U *fBinContent; //!
    const U fZero;

    ClassDef(QTHN,1) //Multidimensional histogram template class
};

#include "QTHN_cxx.h"

#endif
