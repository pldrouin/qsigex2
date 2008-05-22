// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QDISTF_
#define _QDISTF_

#include "Rtypes.h"
#include "TF1.h"
#include "QDis.h"
#include "QTFOps.h"
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
  QDisTF():QDis(){}

  QDisTF(const Char_t* classname):QDis(classname){}

  QDisTF(const QDisTF& newqdtf):QDis(newqdtf.GetClassName()){PRINTF2(this,"\tQDisTF::QDisTF(const QDisTF& newqdtf)\n") *this=newqdtf; }

  QDisTF(const Char_t* classname, const Char_t* filename, const Char_t* objectname):QDis(classname,filename, objectname)
    {
      PRINTF6(this,"\tQDisTF::QDisTF(const Char_t* filename<",filename,">, const Char_t* objectname<",objectname,">)\n")
    }

  QDisTF(const Char_t* classname, const TF1& newobject):QDis(classname,newobject)
    {
      PRINTF2(this,"\tQDisTF::QDisTF(const TF& newobject)\n")
    }

  virtual ~QDisTF(){PRINTF2(this,"\tQDisTF::~QDisTF()\n")}

  const QDisTF& operator=(const QDisTF& newqdis)
  {
    PRINTF2(this,"\tconst QDisTF& QDisTF::operator=(const QDisTF& newqdis)\n")

    QDis::operator=(newqdis);
    return *this;
  }

  QDis* CloneQDis() const
    {
      PRINTF2(this,"\tQDis* QDisTF::CloneQDis()\n")

      return new QDisTF(*this);
    }

  Double_t ProbDensity(const Double_t &x,const Double_t &y=0,const Double_t &z=0) const;
  Double_t Derivative(const Double_t &x) const;

  void Normalize(Double_t* fullintegral=NULL, Double_t* cutintegral=NULL, Double_t* error=NULL);

  Int_t GetDimension(){return dynamic_cast<TF1*>(GetObject())->GetNdim();}

 private:
  static QTFOps fQTFOps; //!

  ClassDef(QDisTF,1) //Derived class from QDis that allows to get probabilities from a TF
};
#include "debugger.h"

#endif
