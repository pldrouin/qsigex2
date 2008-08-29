// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QDISTHN_
#define _QDISTHN_

#include "Rtypes.h"
#include "TFile.h"
#include "QDis.h"
#include "QTHN.h"
#include <iostream>

using std::cout;

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTHN                                                          //
//                                                                  //
// This class creates a probability density function from a TH      //
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

  QDisTHN(const Char_t *name, const Char_t *title, Int_t ndims): QDis(), fOwned(kTRUE), fQTHN(new QTHN<U>(name,title,ndims)){}

  QDisTHN(const QDisTHN<U>& newqdis): QDis(newqdis), fOwned(kTRUE)
  {
    if(newqdis.fQTHN) {
	fQTHN=(QTHN<U>*)newqdis.fQTHN->Clone();
    } else fQTHN=NULL;
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

  operator QTHN<U>&(){return *fQTHN;}

  QDis* CloneQDis() const
    {
      return new QDisTHN<U>(*this);
    }

  void Draw(Option_t*){}

  Int_t Fill(const Double_t &x);
  Int_t Fill(const Double_t &x, const Double_t &y);
  Int_t Fill(const Double_t &x, const Double_t &y, const Double_t &z);
  Int_t Fill(const Double_t *x);
  
  TH1* GenTH(const char *name="_qth2th") const{return fQTHN->GenTH(name);}

  Int_t GetDimension() const{return fQTHN->GetNDims();}

  QTHN<U>* GetTHN() const{return fQTHN;} 

  void InitProcObj(){fQTHN->Reset();}

  Double_t Integral(Int_t** binranges=NULL, Bool_t *widths=NULL) const{return fQTHN->Integral(binranges,widths);}

  void Normalize(Double_t* integral=NULL);

  Double_t ProbDensity(const Double_t &x,const Double_t &y=0,const Double_t &z=0) const;
  Double_t ProbDensity(const Double_t *x) const{return fQTHN->GetBinContent(fQTHN->FindBin(x));}

  QDisTHN<U>* MarginalPDF(const char *name="_md", const Int_t *axes=NULL, Int_t naxes=0) const;

  void SetAxis(Int_t axis, Int_t nbins, Double_t min, Double_t max){fQTHN->SetAxis(axis,nbins,min,max);}
  void SetAxis(Int_t axis, Int_t nbins, Double_t *bins){fQTHN->SetAxis(axis,nbins,bins);}
  void SetAxis(Int_t axis, const TAxis* anaxis){fQTHN->SetAxis(axis,anaxis);}

  void TerminateProcObj(){Normalize(); UpdateModTime();}

 private:
  void SetNameTitleToObject(){SetNameTitle(fQTHN->GetName(),fQTHN->GetTitle());}
  Bool_t fOwned;
  QTHN<U> *fQTHN;

  ClassDef(QDisTHN,1) //Derived class from QDis that allows to get probabilities from a QTHN
};

#include "QDisTHN_cxx.h"

#endif
