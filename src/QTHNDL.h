// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QTHNDL_
#define _QTHNDL_

#include <cstdio>
#include <cstdlib>
#include "QTHNF.h"

template <typename U> class QTHNDL: public QTHNF<U>
{
  public:
    QTHNDL(): QTHNF<U>(), fFBins(NULL){};
    QTHNDL(const QTHNDL &qthn);
    QTHNDL(const QTHNF<U> &qthn): QTHNF<U>(qthn), fFBins(NULL){ComputeNBins();}
    QTHNDL(const QTHN<U> &qthn): QTHNF<U>(qthn), fFBins(NULL){ComputeNBins();}
    QTHNDL(const Char_t *name, const Char_t *title, Int_t ndims): QTHNF<U>(name,title,ndims), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy=0, const Float_t &ylow=0, const Float_t &yhigh=0, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz=0, const Float_t &zlow=0, const Float_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}

    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy=0, const Double_t &ylow=0, const Double_t &yhigh=0, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QTHNF<U>(name,title,nbinsx,xlow,xhigh,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ylow,yhigh,nbinsz,zbins), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz=0, const Double_t &zlow=0, const Double_t &zhigh=0): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zlow,zhigh), fFBins(NULL){}
    QTHNDL(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): QTHNF<U>(name,title,nbinsx,xbins,nbinsy,ybins,nbinsz,zbins), fFBins(NULL){}

    virtual ~QTHNDL(){Clear();}
    void AddBinContent(const Long64_t &bin, const U &w=1);
    void Clear(Option_t* option="");
    TObject* Clone(const char* newname = NULL) const{QTHNDL<U>* ret=new QTHNDL(*this); if(newname) ret->SetName(newname); return ret;}
    Long64_t GetFBin(const Int_t *coords) const{return fFBins[QTHN<U>::GetBin(coords)];}
    Long64_t GetFBin(const Long64_t &bin) const{return fFBins[bin];}
    const U& GetBinContent(const Long64_t &bin) const;
    const QTHNDL<U>& operator=(const QTHNDL<U> &qthn);
    const QTHNDL<U>& operator=(const QTHNF<U> &qthn){Clear(); QTHNF<U>::operator=(qthn); ComputeNBins(); return *this;}
    const QTHNDL<U>& operator=(const QTHN<U> &qthn){Clear(); QTHNF<U>::operator=(qthn); ComputeNBins(); return *this;}
    QTHN<U>* Projection(const char *name="_pd", const Int_t *axes=NULL, const Int_t &naxes=0, QTHN<U> *th=NULL) const;
    void Reset();
    void SetBinContent(const Long64_t &bin, const U &content);
    void SetFBinContent(const Long64_t &fbin, const U &content);
  protected:
    void ComputeNBins();
    Long64_t *fFBins; //!

    ClassDef(QTHNDL,1) //Multidimensional histogram template class optimized for iteration over filled bins, but having a double list to speed up random access of filled bins.
};

#include "QTHNDL_cxx.h"

#endif
