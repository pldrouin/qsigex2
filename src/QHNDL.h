// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QHNDL_
#define _QHNDL_

#include <cstdio>
#include <cstdlib>
#include "QHNF.h"

template <typename U> class QHNDL: public QHNF<U>
{
  public:
    QHNDL(): QHNF<U>(), fFBins(NULL){};
    QHNDL(const QHNDL &qthn);
    QHNDL(const QHNF<U> &qthn): QHNF<U>(qthn), fFBins(NULL){ComputeNBins();}
    QHNDL(const QHN<U> &qthn): QHNF<U>(qthn), fFBins(NULL){ComputeNBins();}
    QHNDL(const Char_t *name, const Char_t *title, Int_t ndims): QHNF<U>(name,title,ndims), fFBins(NULL){}
#ifndef __CINT__
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}
#endif

    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
#ifndef __CINT__
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
#endif
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
#ifndef __CINT__
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
#endif
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
    QHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}

    virtual ~QHNDL(){Clear();}
    void AddBinContent(const Long64_t &bin, const U &w=1);
    void Clear(Option_t* option="");
    TObject* Clone(const char* newname = NULL) const{QHNDL<U>* ret=new QHNDL(*this); if(newname) ret->SetName(newname); return ret;}
    Long64_t GetFBin(const Int_t *coords) const{return fFBins[QHN<U>::GetBin(coords)];}
    Long64_t GetFBin(const Long64_t &bin) const{return fFBins[bin];}
    const U& GetBinContent(const Long64_t &bin) const;
    QHN<U>* New() const{return new QHNDL<U>;}
    QHN<U>* New(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHNDL<U>(name,title,ndims);}
    const QHNDL<U>& operator=(const QHNDL<U> &qthn);
    const QHNDL<U>& operator=(const QHNF<U> &qthn){Clear(); QHNF<U>::operator=(qthn); ComputeNBins(); return *this;}
    const QHNDL<U>& operator=(const QHN<U> &qthn){Clear(); QHNF<U>::operator=(qthn); ComputeNBins(); return *this;}
    void Reset();
    void SetBinContent(const Long64_t &bin, const U &content);
    void SetFBinContent(const Long64_t &fbin, const U &content);
  protected:
    QHN<Double_t>* NewD() const{return new QHNDL<Double_t>;}
    QHN<Double_t>* NewD(const Char_t* name, const Char_t* title, const Int_t &ndims) const{return new QHNDL<Double_t>(name,title,ndims);}
    void ComputeNBins();
    Long64_t *fFBins; //!

    ClassDef(QHNDL,1) //Multidimensional histogram template class optimized for iteration over filled bins, but having a double list to speed up random access of filled bins.
};

typedef QHNDL<Double_t> QHNDL_D;
typedef QHNDL<Float_t> QHNDL_F;
typedef QHNDL<Int_t> QHNDL_I;

#include "QHNDL_cxx.h"

#endif
