// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QDISTH_
#define _QDISTH_

#include "Rtypes.h"
#include "TFile.h"
#include "TH3D.h"
#include "TH3F.h"
#include "TH2D.h"
#include "TH2F.h"
#include "TH1D.h"
#include "TH1F.h"
#include "QDis.h"
#include <iostream>

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTH                                                           //
//                                                                  //
// This class creates a probability density function from a TH      //
// object. The class is derived from abstract base class QDis       //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

class QDisTH: public QDis
{
 public:
  QDisTH(): QDis(), fOwned(kTRUE), fTH(NULL), fNDims(0){}

  QDisTH(const QDisTH& newqdis): QDis(newqdis), fOwned(kTRUE)
  {
    PRINTF2(this,"\tQDisTH::QDisTH(const QDisTH& newqdth)\n")
    if(newqdis.fTH) {
	fTH=(TH1*)newqdis.fTH->Clone();
	fTH->SetDirectory(NULL);
        fNDims=fTH->GetDimension();
    } else {
      fTH=NULL;
      fNDims=0;
    }
  }

  QDisTH(const Char_t* filename, const Char_t* objectname);

  QDisTH(const TH1& newobject): QDis(), fOwned(kTRUE)
  {
    PRINTF2(this,"\tQDisTH::QDisTH(const TH& newobject)\n")
    fTH=(TH1*)newobject.Clone();
    fTH->SetDirectory(NULL);
    fNDims=fTH->GetDimension();
    SetNameTitleToObject();
  }

  QDisTH(TH1 *object): QDis(), fOwned(kFALSE)
  {
    PRINTF2(this,"\tQDisTH::QDisTH(TH *object)\n")
    fTH=object;
    fNDims=fTH->GetDimension();
    SetNameTitleToObject();
  }

  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy=0, Float_t ylow=0, Float_t yhigh=0, Int_t nbinsz=0, Float_t zlow=0, Float_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, Float_t ylow, Float_t yhigh, Int_t nbinsz, const Float_t *zbins);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz=0, Float_t zlow=0, Float_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy=0, Float_t ylow=0, Float_t yhigh=0, Int_t nbinsz=0, Float_t zlow=0, Float_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz, const Float_t *zbins);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy, Float_t ylow, Float_t yhigh, Int_t nbinsz, const Float_t *zbins);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz=0, Float_t zlow=0, Float_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Float_t *xbins, Int_t nbinsy, const Float_t *ybins, Int_t nbinsz, const Float_t *zbins);

  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy=0, Double_t ylow=0, Double_t yhigh=0, Int_t nbinsz=0, Double_t zlow=0, Double_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, Double_t ylow, Double_t yhigh, Int_t nbinsz, const Double_t *zbins);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, const Double_t *ybin, Int_t nbinsz=0, Double_t zlow=0, Double_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t const *xbins, Int_t nbinsy=0, Double_t ylow=0, Double_t yhigh=0, Int_t nbinsz=0, Double_t zlow=0, Double_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy, const Double_t *ybins, Int_t nbinsz, const Double_t *zbins);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Double_t *xbins, Int_t nbinsy, Double_t ylow, Double_t yhigh, Int_t nbinsz, const Double_t *zbins);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Double_t *xbins, Int_t nbinsy, const Double_t *ybins, Int_t nbinsz=0, Double_t zlow=0, Double_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, const Double_t *xbins, Int_t nbinsy, const Double_t *ybins, Int_t nbinsz, const Double_t *zbins);

  virtual ~QDisTH(){PRINTF2(this,"\tQDisTH::~QDisTH()\n") if(fTH && fOwned) delete fTH;}

  const QDisTH& operator=(const QDisTH& newqdis)
  {
    PRINTF2(this,"\tconst QDisTH& QDisTH::operator=(const QDisTH& newqdis)\n")

    QDis::operator=(newqdis);
    if(fTH && fOwned) delete fTH;
    if(newqdis.fTH) {
      fTH=(TH1*)newqdis.fTH->Clone();
      fOwned=kTRUE;
      fNDims=fTH->GetDimension();
      fTH->SetDirectory(NULL);
    } else {
      fTH=NULL;
      fNDims=0;
      fOwned=kTRUE;
    }
    return *this;
  }

  operator TH1&() {return *fTH;}
  operator const TH1&() const{return *fTH;}

  QDis* CloneQDis() const
    {
      PRINTF2(this,"\tQDis* QDisTH::CloneQDis()\n")

      return new QDisTH(*this);
    }

  void Draw(Option_t *option=""){fTH->Draw(option);}

  Double_t Eval(const Double_t &x0) const;
  Double_t Eval(const Double_t &x0, const Double_t &x1) const;
  Double_t Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2) const;
  Double_t Eval(Double_t const* const &x) const;

  void Fill(const Double_t &x);
  void Fill(const Double_t &x, const Double_t &y);
  void Fill(const Double_t &x, const Double_t &y, const Double_t &z);
  
  const Int_t& GetNDims() const{return fNDims;}

  TH1* GetTH() const{return fTH;} 

  void InitProcObj(){fTH->Reset();}

  Double_t Integral(Int_t** binranges=NULL, Bool_t *widths=NULL) const;

  void Normalize(Double_t* integral=NULL, Bool_t reverse=kFALSE);

  QDisTH* MarginalPDF(const char *name="_md", const Int_t xaxis=0, const Int_t yaxis=-1) const;
  //QDisTH* MarginalPDF2D(const char *name="_m1d", Int_t xaxis=0, Int_t yaxis=1) const;

  void TerminateProcObj(){Normalize();}

 protected:
  Bool_t IsConstantBW(const Int_t &nbins, const Double_t *bins) const;
  Bool_t IsConstantBW(const Int_t &nbins, const Float_t *bins) const;

 private:
  void SetNameTitleToObject(){SetNameTitle(fTH->GetName(),fTH->GetTitle());}
  Bool_t fOwned;
  TH1 *fTH;
  Int_t fNDims; //!

  ClassDef(QDisTH,2) //Derived class from QDis that allows to get probabilities from a TH
};

#include "debugger.h"

#endif
