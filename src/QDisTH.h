// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QDISTH_
#define _QDISTH_

#include "Rtypes.h"
#include "TH3.h"
#include "QTHOps.h"
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
  QDisTH():QDis(){}

  QDisTH(const Char_t* classname):QDis(classname){}

  QDisTH(const QDisTH& newqdth):QDis(newqdth.GetClassName()){PRINTF2(this,"\tQDisTH::QDisTH(const QDisTH& newqdth)\n") *this=newqdth; }

  QDisTH(const Char_t* classname, const Char_t* filename, const Char_t* objectname):QDis(classname,filename, objectname)
    {
      PRINTF8(this,"\tQDisTH::QDisTH(const Char_t* classname<",classname,">, const Char_t* filename<",filename,">, const Char_t* objectname<",objectname,">)\n")
    }

  QDisTH(const Char_t* classname, const TH1& newobject):QDis(classname,newobject)
    {
      PRINTF4(this,"\tQDisTH::QDisTH(const Char_t* classname<",classname,">, const TH& newobject)\n")
    }

  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t xlow, Float_t xhigh, Int_t nbinsy=0, Float_t ylow=0, Float_t yhigh=0, Int_t nbinsz=0, Float_t zlow=0, Float_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Float_t *xbins, Int_t nbinsy=0, Float_t *ybins=NULL, Int_t nbinsz=0, Float_t *zbins=NULL);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t xlow, Double_t xhigh, Int_t nbinsy=0, Double_t ylow=0, Double_t yhigh=0, Int_t nbinsz=0, Double_t zlow=0, Double_t zhigh=0);
  QDisTH(const Char_t *name, const Char_t *title, Int_t nbinsx, Double_t *xbins, Int_t nbinsy=0, Double_t *ybins=NULL, Int_t nbinsz=0, Double_t *zbins=NULL);

  virtual ~QDisTH(){PRINTF2(this,"\tQDisTH::~QDisTH()\n")}

  const QDisTH& operator=(const QDisTH& newqdis)
  {
    PRINTF2(this,"\tconst QDisTH& QDisTH::operator=(const QDisTH& newqdis)\n")

    QDis::operator=(newqdis);
    return *this;
  }

  operator TH1&(){return *(dynamic_cast<TH1*>(GetObject()));}

  QDis* CloneQDis() const
    {
      PRINTF2(this,"\tQDis* QDisTH::CloneQDis()\n")

      return new QDisTH(*this);
    }

  Int_t Fill(const Double_t &x);
  Int_t Fill(const Double_t &x, const Double_t &y);
  Int_t Fill(const Double_t &x, const Double_t &y, const Double_t &z);
  
  Int_t GetDimension(){return dynamic_cast<TH1*>(GetObject())->GetDimension();}

  void InitProcObj(){((TH1*)GetObject())->Reset();}

  void Normalize(Double_t* fullintegral=NULL, Double_t* cutintegral=NULL, Double_t* error=NULL);

  Double_t ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const;

  void TerminateProcObj(){Normalize(); UpdateModTime();}

 private:
  static QTHOps fQTHOps; //!

  ClassDef(QDisTH,1) //Derived class from QDis that allows to get probabilities from a TH
};

#include "debugger.h"

#endif
