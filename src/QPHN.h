// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QPHN_
#define _QPHN_

#include <cstdio>
#include <cstdlib>
#include "QHN.h"
#include "QHNF.h"
#include "QHNDL.h"
#include "QPHData.h"

template <typename DTYPE=QPHData, typename QHBASE=QHN<DTYPE> > class QPHN: public QHBASE
{
  public:
    QPHN(): QHBASE(){};
    QPHN(const QPHN &qthn): QHBASE(qthn){}
    QPHN(const Char_t *name, const Char_t *title, Int_t ndims): QHBASE(name,title,ndims){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){}

    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){}

    virtual ~QPHN(){}
    inline virtual void AddBinContent(const Long64_t &bin, const Double_t &y){QHBASE::AddBinContent(bin, DTYPE(y,1));}
    inline virtual void AddBinContent(const Long64_t &bin, const Double_t &y, const Double_t &w){QHBASE::AddBinContent(bin, DTYPE(y,w));}
    TObject* Clone(const char* newname = "") const{QPHN<DTYPE, QHBASE>* ret=new QPHN<DTYPE, QHBASE>(*this); if(strdiffer(newname,"")) dynamic_cast<TNamed*>(ret)->SetName(newname); return dynamic_cast<TObject*>(ret);}
    inline void Fill(Double_t const * const &x, const Double_t &y){AddBinContent(QHBASE::FindBin(x),y);}
    inline void Fill(Double_t const * const &x, const Double_t &y, const Double_t &w){AddBinContent(QHBASE::FindBin(x),y,w);}
    inline void Fill(const Double_t &x0, const Double_t &y){AddBinContent(QHBASE::FindBin(x0),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6,x7),y);}
    inline void Fill(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8),y);}
    inline void Fill(Float_t const * const &x, const Double_t &y){AddBinContent(QHBASE::FindBin(x),y);}
    inline void Fill(Float_t const * const &x, const Double_t &y, const Double_t &w){AddBinContent(QHBASE::FindBin(x),y,w);}
    inline void Fill(const Float_t &x0, const Float_t &y){AddBinContent(QHBASE::FindBin(x0),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6,x7),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8),y);}
    QHBASE* New() const{return new QPHN<DTYPE, QHBASE>;}
    QHBASE* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QPHN<DTYPE, QHBASE>(name,title,ndims);}
  protected:
    QHN_D* NewD() const{return new QHN_D();}
    QHN_D* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHN_D(name,title,ndims);}
  private:
    inline void AddBinContent(const Long64_t &fbin){}
    void Fill(const Double_t &x0){}
    void Fill(const Float_t &x0){}

    ClassDef(QPHN,1) //Multidimensional profile histogram class optimized for random access
};

typedef QPHN<QPHData,QHNF<QPHData> > QPHNF;
typedef QPHN<QPHData, QHNDL<QPHData> > QPHNDL;
typedef QPHN<QPHEData,QHNF<QPHEData> > QPHENF;
typedef QPHN<QPHEData, QHNDL<QPHEData> > QPHENDL;

#include "QPHN_Dict_cxx.h"

#endif
