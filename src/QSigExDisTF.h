// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXDISTF_
#define _QSIGEXDISTF_

#include "Rtypes.h"
#include "TF1.h"
#include "QSigExDis.h"
#include "QSigExTFOps.h"
#include <iostream>

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QSigEXDisTF                                                      //
//                                                                  //
// This class creates a probability density function from a TF      //
// object. The class is derived from abstract base class QSigExDis  //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

class QSigExDisTF: public QSigExDis
{
 public:
  QSigExDisTF():QSigExDis(){}

  QSigExDisTF(const Char_t* classname):QSigExDis(classname){}

  QSigExDisTF(const QSigExDisTF& newqdtf):QSigExDis(newqdtf.GetClassName()){PRINTF2(this,"\tQSigExDisTF::QSigExDisTF(const QSigExDisTF& newqdtf)\n") *this=newqdtf; }

  QSigExDisTF(const Char_t* classname, const Char_t* filename, const Char_t* objectname):QSigExDis(classname,filename, objectname)
    {
      PRINTF6(this,"\tQSigExDisTF::QSigExDisTF(const Char_t* filename<",filename,">, const Char_t* objectname<",objectname,">)\n")
    }

  QSigExDisTF(const Char_t* classname, const TF1& newobject):QSigExDis(classname,newobject)
    {
      PRINTF2(this,"\tQSigExDisTF::QSigExDisTF(const TF& newobject)\n")
    }

  virtual ~QSigExDisTF(){PRINTF2(this,"\tQSigExDisTF::~QSigExDisTF()\n")}

  const QSigExDisTF& operator=(const QSigExDisTF& newqdis)
  {
    PRINTF2(this,"\tconst QSigExDisTF& QSigExDisTF::operator=(const QSigExDisTF& newqdis)\n")

    QSigExDis::operator=(newqdis);
    return *this;
  }

  QSigExDis* CloneQSigExDis() const
    {
      PRINTF2(this,"\tQSigExDis* QSigExDisTF::CloneQSigExDis()\n")

      return new QSigExDisTF(*this);
    }

  Double_t ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const;
  Double_t Derivative(const Double_t &x) const;

  void Normalize(Option_t* cutexpr=NULL, Int_t normflags=0, Double_t* fullintegral=NULL,
		 Double_t* cutintegral=NULL, Double_t* error=NULL);

  Int_t GetDimension(){return dynamic_cast<TF1*>(GetObject())->GetNdim();}

 private:
  static QSigExTFOps fQSigExTFOps; //!

  ClassDef(QSigExDisTF,1) //Derived class from QSigExDis that allows to get probabilities from a TF
};
#include "debugger.h"

#endif
