// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXDIS_
#define _QSIGEXDIS_

#include <iostream>
#include "TNamed.h"
#include "TCollection.h"
#include "TFormula.h"
#include "TVirtualPad.h"
#include "TDirectory.h"
#include "QSigExIO.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExDis                                                            //
//                                                                      //
// This class is an abstract base class which allows to give a common   //
// interface to all types of ROOT "functions" (TH1, TH2, TH3, TF1, TF2, //
// TF3) such that they can be transparently used as probability density //
// functions. QSigExDis declares a Normalize function that allows to    //
// normalize the functions using complexe cuts and a ProbDensity        //
// function that returns the probability density associated with a      //
// given point.                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QSigExDis: public TNamed, public QSigExIO{
 public:
  QSigExDis():QSigExIO()
    {
      PRINTF2(this,"\tQSigExDis::QSigExDis()\n")
    }
  QSigExDis(const Char_t* classname):QSigExIO(classname){}
  QSigExDis(const Char_t* classname, const Char_t* filename, const Char_t* objectname):QSigExIO(classname, filename,objectname){}
  QSigExDis(const Char_t* classname, const TObject& rhs):QSigExIO(classname, rhs){}
  
  virtual ~QSigExDis();

  virtual Double_t ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const=0;
  virtual Double_t Derivative(const Double_t &x) const {return 0;};
  

  virtual QSigExDis* CloneQSigExDis() const=0;

  virtual void Normalize(Option_t* cutexpr=NULL,
      			 Int_t     normflags=0,
			 Double_t* fullintegral=NULL,
			 Double_t* cutintegral=NULL,
			 Double_t* error=NULL)=0;

  const QSigExDis& operator=(const QSigExDis &rhs){TNamed::operator=(rhs); QSigExIO::operator=(dynamic_cast<const QSigExIO&>(rhs)); return *this;}

  void Draw(Option_t *option=""){GetObject()->Draw(option);}

  virtual void Browse(TBrowser* b)
    {
      b=NULL;
      Draw();
      gPad->Update();
    }

  virtual Int_t GetDimension()=0;

 private:
  QSigExDis(const QSigExDis& rhs): TNamed(rhs),QSigExIO(rhs.GetClassName()){*this=rhs;}

  ClassDef(QSigExDis,1) //Abstract class of QSigExDis* classes
};

#include "debugger.h"
#endif
