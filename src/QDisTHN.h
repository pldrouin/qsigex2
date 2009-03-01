// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QDISTHN_
#define _QDISTHN_

#include "Rtypes.h"
#include "TFile.h"
#include "QDis.h"
#include "QTHN.h"
#include "QTHNF.h"
#include "QTHNDL.h"
#include <iostream>

using std::cout;

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTHN                                                          //
//                                                                  //
// This class creates a probability density function from a THN     //
// object. The class is derived from abstract base class QDis       //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

template <typename U> class QDisTHN: public QDis
{
 public:
  QDisTHN(): QDis(), fOwned(kTRUE), fQTHN(NULL){}

  enum eQTHNTypes {kQTHN, kQTHNF, kQTHNDL};

  QDisTHN(const Char_t *name, const Char_t *title, const Int_t &ndims, const Int_t &qthntype=kQTHNF);

  QDisTHN(const QDisTHN<U>& newqdis): QDis(newqdis), fOwned(kTRUE)
  {
    if(newqdis.fQTHN) {
	fQTHN=(QTHN<U>*)newqdis.fQTHN->Clone();
    } else fQTHN=NULL;
    SetNameTitleToObject();
  }

  QDisTHN(const QTHN<U>& newobject): QDis(), fOwned(kTRUE)
  {
    fQTHN=(QTHN<U>*)newobject.Clone();
    SetNameTitleToObject();
  }

  QDisTHN(QTHN<U> *object): QDis(), fOwned(kFALSE)
  {
    fQTHN=object;
    SetNameTitleToObject();
  }

  virtual ~QDisTHN(){if(fQTHN && fOwned) delete fQTHN;}

  const QDisTHN<U>& operator=(const QDisTHN<U>& newqdis)
  {

    QDis::operator=(newqdis);
    if(fQTHN && fOwned) delete fQTHN;
    if(newqdis.fQTHN) {
      fQTHN=(QTHN<U>*)newqdis.fQTHN->Clone();
      fOwned=kTRUE;
    } else {
      fQTHN=NULL;
      fOwned=kTRUE;
    }
    return *this;
  }

  operator QTHN<U>&() {return *fQTHN;}
  operator const QTHN<U>&() const{return *fQTHN;}

  QDis* CloneQDis() const
    {
      return new QDisTHN<U>(*this);
    }

  void Draw(Option_t *option=""){fQTHN->Draw(option);}

  template <typename V> void Fill(const V &x){fQTHN->AddBinContent(fQTHN->FindBin(x));}
  template <typename V> void Fill(const V &x, const V &y){fQTHN->AddBinContent(fQTHN->FindBin(x,y));}
  template <typename V> void Fill(const V &x, const V &y, const V &z){fQTHN->AddBinContent(fQTHN->FindBin(x,y,z));}
  void Fill(const Double_t *x, const U &w=1){fQTHN->Fill(x,w);}
  
  TH1* GenTH(const char *name="_qth2th") const{return fQTHN->GenTH(name);}

  Int_t GetDimension() const{return fQTHN->GetNDims();}

  QTHN<U>* GetTHN() const{return fQTHN;} 

  void InitProcObj(){fQTHN->Reset();}

  Double_t Integral(const Int_t** binranges=NULL, const Bool_t *widths=NULL) const{return fQTHN->Integral(binranges,widths);}

  void Normalize(Double_t* integral=NULL);

  Double_t ProbDensity(const Double_t &x,const Double_t &y=0,const Double_t &z=0) const;
  Double_t ProbDensity(const Double_t *x) const{return fQTHN->GetBinContent(fQTHN->FindBin(x));}

  QDisTHN<U>* MarginalPDF(const char *name="_md", const Int_t *axes=NULL, const Int_t &naxes=0) const;

  void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t &min, const Double_t &max){fQTHN->SetAxis(axis,nbins,min,max);}
  void SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t *bins){fQTHN->SetAxis(axis,nbins,bins);}
  void SetAxis(const Int_t &axis, const QAxis* anaxis){fQTHN->SetAxis(axis,anaxis);}

  void TerminateProcObj(){Normalize();}

 private:
  void SetNameTitleToObject(){SetNameTitle(fQTHN->GetName(),fQTHN->GetTitle());}
  Bool_t fOwned;
  QTHN<U> *fQTHN;

  ClassDef(QDisTHN,1) //Derived class from QDis that allows to get probabilities from a QTHN
};

#include "QDisTHN_cxx.h"

#endif
