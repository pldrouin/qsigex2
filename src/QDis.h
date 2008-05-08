// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QDIS_
#define _QDIS_

#include <iostream>
#include "TNamed.h"
#include "TCollection.h"
#include "TFormula.h"
#include "TVirtualPad.h"
#include "TDirectory.h"
#include "TTimeStamp.h"
#include "QTObjectIO.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QDis                                                                 //
//                                                                      //
// This class is an abstract base class which allows to give a common   //
// interface to all types of ROOT "functions" (TH1, TH2, TH3, TF1, TF2, //
// TF3) such that they can be transparently used as probability density //
// functions. QDis declares a Normalize function that allows to         //
// normalize the functions using complexe cuts and a ProbDensity        //
// function that returns the probability density associated with a      //
// given point.                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QDis: public TNamed, public QTObjectIO
{
 public:
  QDis():QTObjectIO(), fCutExpr(), fNormFlags(0)
    {
      PRINTF2(this,"\tQDis::QDis()\n")
    }
  QDis(const Char_t* classname):QTObjectIO(classname), fCutExpr(), fNormFlags(0), fLastModified(){}
  QDis(const Char_t* classname, const Char_t* filename, const Char_t* objectname):QTObjectIO(classname, filename,objectname), fCutExpr(), fNormFlags(0), fLastModified(){SetNameTitleToObject();}
  QDis(const Char_t* classname, const TObject& rhs):QTObjectIO(classname, rhs), fCutExpr(), fNormFlags(0), fLastModified(){SetNameTitleToObject();}
  
  virtual ~QDis();

  virtual Double_t ProbDensity(const Double_t &x,const Double_t &y,const Double_t &z) const=0;
  virtual Double_t Derivative(const Double_t&) const {return 0;};
  

  virtual QDis* CloneQDis() const=0;

  virtual void Normalize(Double_t* fullintegral=NULL,
			 Double_t* cutintegral=NULL,
			 Double_t* error=NULL)=0;

  const QDis& operator=(const QDis &rhs){
    TNamed::operator=(rhs);
    QTObjectIO::operator=(dynamic_cast<const QTObjectIO&>(rhs));
    fCutExpr=rhs.fCutExpr;
    fNormFlags=rhs.fNormFlags;
    fLastModified=rhs.fLastModified;
    return *this;}

  void Draw(Option_t *option=""){GetObject()->Draw(option);}

  virtual void Browse(TBrowser* b)
    {
      b=NULL;
      Draw();
      gPad->Update();
    }

  virtual Int_t GetDimension()=0;

  const Option_t* GetCutExpr() const{return fCutExpr;}
  const TTimeStamp& GetModTime() const{return fLastModified;}
  Int_t GetNormFlags() const{return fNormFlags;}

  void SetCutExpr(Option_t *cutexpr=NULL){fCutExpr=cutexpr;}
  void SetNormFlags(Int_t normflags=0){fNormFlags=normflags;}

  void UpdateModTime(){fLastModified.Set();}

 protected:
  void SetNameTitleToObject();
  TString fCutExpr;
  Int_t fNormFlags;
  TTimeStamp fLastModified;

 private:
  QDis(const QDis& rhs): TNamed(rhs),QTObjectIO(rhs.GetClassName()){*this=rhs;}

  ClassDef(QDis,1) //Abstract class of QDis* classes
};

#include "debugger.h"
#endif
