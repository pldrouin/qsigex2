// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QPHN_
#define _QPHN_

#include <cstdio>
#include <cstdlib>
#include "TMath.h"
#include "strdiffer.h"
#include "QAxis.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"
#include "QHN.h"
#include "QPHData.h"

typedef QHN<QPHData> QHN_QPHD;

class QPHN: public QHN_QPHD
{
  public:
    QPHN(): QHN_QPHD(){};
    QPHN(const QPHN &qthn): QHN_QPHD(qthn){}
    QPHN(const Char_t *name, const Char_t *title, Int_t ndims): QHN_QPHD(name,title,ndims){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){}

    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN_QPHD(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHN_QPHD(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){}

    virtual ~QPHN(){}
    inline void AddBinContent(const Long64_t &bin, const Double_t &y)
    {
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QPHN::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      QHN_QPHD::fBinContent[bin].Add(y); ++QHN_QPHD::fEntries;
    }
    inline void AddBinContent(const Long64_t &bin, const Double_t &y, const Double_t &w)
    {
#ifndef QSFAST
      if(bin<0 || bin>=fNBins) {
	fprintf(stderr,"Error: QPHN::AddBinContent: %lli is not a valid bin number\n",bin);
	throw 1;
      }
#endif
      QHN_QPHD::fBinContent[bin].Add(w*y,w); QHN_QPHD::fEntries+=w;
    }
    TObject* Clone(const char* newname = "") const{QPHN* ret=new QPHN(*this); if(strdiffer(newname,"")) dynamic_cast<TNamed*>(ret)->SetName(newname); return dynamic_cast<TObject*>(ret);}
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
    QHN_QPHD* New() const{return new QPHN;}
    QHN_QPHD* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QPHN(name,title,ndims);}
  protected:
    QHN_D* NewD() const{return new QHN_D();}
    QHN_D* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHN_D(name,title,ndims);}
  private:
    void AddBinContent(const Long64_t &fbin){}
    void Fill(const Double_t &x0){}
    void Fill(const Float_t &x0){}
    const QPHN& operator=(const QHN_QPHD &qthn){return *this;}

    ClassDef(QPHN,1) //Multidimensional profile histogram class optimized for random access
};

#endif
