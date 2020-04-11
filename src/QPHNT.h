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

template <typename DTYPE=QPHData, typename QHBASE=QHN<DTYPE> > class QPHNT: public QHBASE
{
  public:
    QPHNT(): QHBASE(){};
    QPHNT(const QPHNT &qthn): QHBASE(qthn){}
    QPHNT(const Char_t *name, const Char_t *title, Int_t ndims): QHBASE(name,title,ndims){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){}

    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh){}
    QPHNT(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHBASE(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins){}

    virtual ~QPHNT(){}
    inline virtual void AddBinContent(const Long64_t &bin, const Double_t &y){QHBASE::AddBinContent(bin, DTYPE::Generator(y));}
    TObject* Clone(const char* newname = "") const{QPHNT<DTYPE, QHBASE>* ret=new QPHNT<DTYPE, QHBASE>(*this); if(strdiffer(newname,"")) dynamic_cast<TNamed*>(ret)->SetName(newname); return dynamic_cast<TObject*>(ret);}
    inline void Fill(Double_t const * const &x, const Double_t &y){AddBinContent(QHBASE::FindBin(x),y);}
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
    inline void Fill(const Float_t &x0, const Float_t &y){AddBinContent(QHBASE::FindBin(x0),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6,x7),y);}
    inline void Fill(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &y){AddBinContent(QHBASE::FindBin(x0,x1,x2,x3,x4,x5,x6,x7,x8),y);}
    QHBASE* New() const{return new QPHNT<DTYPE, QHBASE>;}
    QHBASE* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QPHNT<DTYPE, QHBASE>(name,title,ndims);}
  protected:
    QHN_D* NewD() const{return new QHN_D();}
    QHN_D* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHN_D(name,title,ndims);}
  private:
    inline void AddBinContent(const Long64_t &fbin){}
    void Fill(const Double_t &x0){}
    void Fill(const Float_t &x0){}

    ClassDef(QPHNT,1) //Multidimensional profile histogram class optimized for random access
};

typedef QPHNT<QPHData,QHN<QPHData> > QPHN;
typedef QPHNT<QPHData,QHNF<QPHData> > QPHNF;
typedef QPHNT<QPHData, QHNDL<QPHData> > QPHNDL;
typedef QPHNT<QPHEData,QHN<QPHEData> > QPHEN;
typedef QPHNT<QPHEData,QHNF<QPHEData> > QPHENF;
typedef QPHNT<QPHEData, QHNDL<QPHEData> > QPHENDL;

#include "QPHNT_Dict_cxx.h"

#endif
