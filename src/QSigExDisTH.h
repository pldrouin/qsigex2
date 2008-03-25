// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXDISTH_
#define _QSIGEXDISTH_

#include "Rtypes.h"
#include "TH1.h"
#include "QSigExTHOps.h"
#include "QSigExDis.h"
#include <iostream>

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////
//                                                                  //
// QSigExDisTH                                                      //
//                                                                  //
// This class creates a probability density function from a TH      //
// object. The class is derived from abstract base class QSigExDis  //
// that gives a common interface to all type of p.d.f.. It          //
// implements a Normalize function that allows to normalize the     //
// initial function using complex cuts.                             //
//                                                                  //
//////////////////////////////////////////////////////////////////////

class QSigExDisTH: public QSigExDis
{
 public:
  QSigExDisTH():QSigExDis(){}

  QSigExDisTH(const Char_t* classname):QSigExDis(classname){}

  QSigExDisTH(const QSigExDisTH& newqdth):QSigExDis(newqdth.GetClassName()){PRINTF2(this,"\tQSigExDisTH::QSigExDisTH(const QSigExDisTH& newqdth)\n") *this=newqdth; }

  QSigExDisTH(const Char_t* classname, const Char_t* filename, const Char_t* objectname):QSigExDis(classname,filename, objectname)
    {
      PRINTF8(this,"\tQSigExDisTH::QSigExDisTH(const Char_t* classname<",classname,">, const Char_t* filename<",filename,">, const Char_t* objectname<",objectname,">)\n")
    }

  QSigExDisTH(const Char_t* classname, const TH1& newobject):QSigExDis(classname,newobject)
    {
      PRINTF4(this,"\tQSigExDisTH::QSigExDisTH(const Char_t* classname<",classname,">, const TH& newobject)\n")
    }

  virtual ~QSigExDisTH(){PRINTF2(this,"\tQSigExDisTH::~QSigExDisTH()\n")}

  const QSigExDisTH& operator=(const QSigExDisTH& newqdis)
  {
    PRINTF2(this,"\tconst QSigExDisTH& QSigExDisTH::operator=(const QSigExDisTH& newqdis)\n")

    QSigExDis::operator=(newqdis);
    return *this;
  }

  QSigExDis* CloneQSigExDis() const
    {
      PRINTF2(this,"\tQSigExDis* QSigExDisTH::CloneQSigExDis()\n")

      return new QSigExDisTH(*this);
    }

  Double_t ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const;

  void Normalize(Option_t* cutexpr=NULL, Int_t normflags=0, Double_t* fullintegral=NULL,
		 Double_t* cutintegral=NULL, Double_t* error=NULL);
  
  Int_t GetDimension(){return dynamic_cast<TH1*>(GetObject())->GetDimension();}

 private:
  static QSigExTHOps fQSigExTHOps; //!

  ClassDef(QSigExDisTH,1) //Derived class from QSigExDis that allows to get probabilities from a TH
};

#include "debugger.h"

#endif
