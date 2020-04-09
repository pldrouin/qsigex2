// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QPHNE_
#define _QPHNE_

#include <cstdio>
#include <cstdlib>
#include "TMath.h"
#include "strdiffer.h"
#include "QAxis.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "QHN.h"

class QPHNE: public QHN_D
{
  public:
    QPHNE(): QHN_D(), fBinEntries(NULL), fBinContent2(NULL){};
    QPHNE(const QPHNE &qthn): QHN_D(qthn){fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t)); memcpy(fBinEntries,qthn.fBinEntries,fNBins*sizeof(Double_t)); fBinContent2=(Double_t*)malloc(fNBins*sizeof(Double_t)); memcpy(fBinContent2,qthn.fBinContent2,fNBins*sizeof(Double_t));}
    QPHNE(const Char_t *name, const Char_t *title, Int_t ndims): QHN_D(name,title,ndims), fBinEntries(NULL), fBinContent2(NULL){}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){Init();}

    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){Init();}
    QPHNE(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN_D(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){Init();}

    virtual ~QPHNE(){}
    inline void AddBinContent(const Long64_t &bin, const Double_t &y)
    {
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QPHNE::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      QHN_D::fBinContent[bin]+=y; fBinContent2[bin]+=y*y; ++(fBinEntries[bin]); ++QHN_D::fEntries;
    }
    inline void AddBinContent(const Long64_t &bin, const Double_t &y, const Double_t &w)
    {
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QPHNE::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      QHN_D::fBinContent[bin]+=w*y; fBinContent2[bin]+=w*y*y; fBinEntries[bin]+=w; QHN_D::fEntries+=w;
    }
    void CopyStruct(const QHN_D &qthn);
    void Clear(Option_t* option=""){QHN_D::Clear(); if(fBinEntries) {free(fBinEntries); fBinEntries=NULL; free(fBinContent2); fBinContent2=NULL;}}
    TObject* Clone(const char* newname = "") const{QPHNE* ret=new QPHNE(*this); if(strdiffer(newname,"")) dynamic_cast<TNamed*>(ret)->SetName(newname); return dynamic_cast<TObject*>(ret);}
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
    inline const Double_t GetBinVariance(const Long64_t &bin) const{if(!fBinEntries[bin]) return 0; const Double_t dbuf=fBinContent[bin]/fBinEntries[bin]; return fBinContent2[bin]/fBinEntries[bin]-dbuf*dbuf;}
    QHN_D* New() const{return new QPHNE;}
    QHN_D* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QPHNE(name,title,ndims);}
    const QPHNE& operator=(const QPHNE &qthn);
    inline void Reset(){QHN_D::Reset(); memset(fBinEntries,0,fNBins*sizeof(Double_t)); memset(fBinContent2,0,fNBins*sizeof(Double_t));}
  protected:
    QHN_D* NewD() const{return New();}
    QHN_D* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return New(name,title,ndims);}
    void ComputeNBins(){QHN_D::ComputeNBins(); if(fBinEntries){free(fBinEntries); free(fBinContent2);} fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t)); fBinContent2=(Double_t*)malloc(fNBins*sizeof(Double_t));}
    void Init(){fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t)); memset(fBinEntries,0,fNBins*sizeof(Double_t)); fBinContent2=(Double_t*)malloc(fNBins*sizeof(Double_t)); memset(fBinContent2,0,fNBins*sizeof(Double_t));}
    QHN_D* Projection(const char *name="_pd", const Int_t *axes=NULL, const Int_t &naxes=0) const;
    inline void Scale(const Double_t &scale){const double_t scale2=scale*scale; for(Long64_t li=GetNFbins()-1; li>=0; --li) { fBinContent[li]*=scale; fBinContent2[li]*=scale2;}}

    inline void ScaleBinContent(const Long64_t &bin, const Double_t &scale){
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QPHNE::ScaleBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      Long64_t fbin=GetFBin(bin);
      if(fbin!=-1) {
	fBinContent[fbin]*=scale;
	fBinContent2[fbin]*=scale*scale;
      }
    }
    inline void ScaleBinContent(const Int_t *coords, const Double_t &scale){
      Long64_t fbin=GetFBin(coords);
      if(fbin!=-1) {
	fBinContent[fbin]*=scale;
	fBinContent2[fbin]*=scale*scale;
      }
    }
    inline void ScaleFBinContent(const Long64_t &fbin, const Double_t &scale){
#ifndef QSFAST
      if(fbin<0 || fbin>=fNBins) {
	fprintf(stderr,"Error: QPHNE::ScaleFBinContent: %lli is not a valid bin number\n",fbin);
	throw 1;
      }
#endif
      fBinContent[fbin]*=scale;
      fBinContent2[fbin]*=scale*scale;
    }
  private:
    Double_t *fBinEntries;
    Double_t *fBinContent2;
    mutable Double_t lBCret;
    void AddFBinContent(const Long64_t &fbin, const Double_t &w){}
    void Fill(const Double_t &x0){}
    void Fill(const Float_t &x0){}
    const Bool_t& IsReadThreadSafe() const{return kFALSE;}
    const QPHNE& operator=(const QHN_D &qthn){return *this;}
    void SetBinContent(const Long64_t &bin, const Double_t &content){}
    void SetBinContent(const Int_t *coords, const Double_t &content){}

    ClassDef(QPHNE,1) //Multidimensional profile histogram class optimized for random access
};

#endif
