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
  
  Int_t GetDimension() const{return fQTHN->GetNDims();}

  QTHN<U>* GetTHN() const{return fQTHN;} 

  void InitProcObj(){fQTHN->Reset();}

  Double_t Integral(Int_t** binranges=NULL, Bool_t *widths=NULL) const;

  void Normalize(Double_t* integral=NULL);

  Double_t ProbDensity(const Double_t &x,const Double_t &y=0,const Double_t &z=0) const;
  Double_t ProbDensity(const Double_t *x) const{return fQTHN->GetBinContent(fQTHN->FindBin(x));}

  QDisTHN<U>* MarginalPDF(const char *name="_m1d", Int_t xaxis=0, Int_t yaxis=-1) const;

  void TerminateProcObj(){Normalize(); UpdateModTime();}

 private:
  void SetNameTitleToObject(){SetNameTitle(fQTHN->GetName(),fQTHN->GetTitle());}
  Bool_t fOwned;
  QTHN<U> *fQTHN;

  ClassDef(QDisTHN,1) //Derived class from QDis that allows to get probabilities from a QTHN
};

#include "QDisTHN_cxx.h"

#endif
