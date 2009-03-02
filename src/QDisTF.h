// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QDISTF_
#define _QDISTF_

#include "Rtypes.h"
#include "TFile.h"
#include "TF1.h"
#include "QDis.h"
#include <iostream>

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QDisTF                                                           //
//                                                                  //
// This class creates a probability density function from a TF      //
// object. The class is derived from abstract base class QDis       //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

class QDisTF: public QDis
{
 public:
  QDisTF(): QDis(), fTF(NULL), fNDims(0){}

  QDisTF(const QDisTF& newqdis): QDis(newqdis)
  {
    PRINTF2(this,"\tQDisTF::QDisTF(const QDisTF& newqdis)\n")
    if(newqdis.fTF) {
      fTF=(TF1*)newqdis.fTF->Clone();
      fNDims=fTF->GetNdim();

    } else {
      fTF=NULL;
      fNDims=0;
    }
  }

  QDisTF(const Char_t* filename, const Char_t* objectname);

  QDisTF(const TF1& newobject)
  {
    PRINTF2(this,"\tQDisTF::QDisTF(const TF& newobject)\n")
    fTF=(TF1*)newobject.Clone();
    fNDims=fTF->GetNdim();
    SetNameTitleToObject();
  }

  virtual ~QDisTF(){PRINTF2(this,"\tQDisTF::~QDisTF()\n") if(fTF) delete fTF;}

  const QDisTF& operator=(const QDisTF& newqdis)
  {
    PRINTF2(this,"\tconst QDisTF& QDisTF::operator=(const QDisTF& newqdis)\n")

    QDis::operator=(newqdis);
    if(fTF) delete fTF;
    if(newqdis.fTF) {
      fTF=(TF1*)newqdis.fTF->Clone();
      fNDims=fTF->GetNdim();
    } else {
      fTF=NULL;
      fNDims=0;
    }
    return *this;
  }

  operator TF1&(){return *fTF;}

  QDis* CloneQDis() const
    {
      PRINTF2(this,"\tQDis* QDisTF::CloneQDis()\n")

      return new QDisTF(*this);
    }

  void Draw(Option_t *option=""){fTF->Draw(option);}

  Double_t Eval(const Double_t &x0) const;
  Double_t Eval(const Double_t &x0, const Double_t &x1) const;
  Double_t Eval(const Double_t &x0, const Double_t &x1, const Double_t &x2) const;
  Double_t Eval(Double_t const* const &x) const;

  Double_t Integral(Double_t xlo,Double_t xhi,Double_t ylo=0,Double_t yhi=0, Double_t zlo=0, Double_t zhi=0) const; 
  Double_t Integral(Option_t* domain) const;

  void Normalize(Double_t* integral=NULL);

  const Int_t& GetNDims() const{return fNDims;}

  TF1* GetTF() const{return fTF;}

 private:
  void SetNameTitleToObject(){SetNameTitle(fTF->GetName(),fTF->GetTitle());}
  mutable TF1 *fTF;
  mutable Int_t fNDims; //!
  mutable Double_t fEval; //!
  ClassDef(QDisTF,2) //Derived class from QDis that allows to get probabilities from a TF
};
#include "debugger.h"

#endif
