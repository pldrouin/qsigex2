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
#include "QProcObj.h"

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

class QDis: public TNamed, public QTObjectIO, public QProcObj
{
 public:
  QDis():QTObjectIO(), fCutExpr(), fNormFlags(kRegularNorm)
    {
      PRINTF2(this,"\tQDis::QDis()\n")
    }
  QDis(const Char_t* classname):QTObjectIO(classname), QProcObj(), fCutExpr(), fNormFlags(kRegularNorm){}
  QDis(const Char_t* classname, const Char_t* filename, const Char_t* objectname):QTObjectIO(classname, filename,objectname), QProcObj(), fCutExpr(), fNormFlags(kRegularNorm){SetNameTitleToObject();}
  QDis(const Char_t* classname, const TObject& rhs):QTObjectIO(classname, rhs), QProcObj(), fCutExpr(), fNormFlags(kRegularNorm){SetNameTitleToObject();}
  
  virtual ~QDis();

  virtual Double_t ProbDensity(const Double_t &x,const Double_t &y=0,const Double_t &z=0) const=0;
  virtual Double_t Derivative(const Double_t&) const {return 0;};
  

  virtual QDis* CloneQDis() const=0;

  virtual void Normalize(Double_t* integral=NULL)=0;

  const QDis& operator=(const QDis &rhs){
    TNamed::operator=(rhs);
    QTObjectIO::operator=(dynamic_cast<const QTObjectIO&>(rhs));
    fCutExpr=rhs.fCutExpr;
    fNormFlags=rhs.fNormFlags;
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
  Int_t GetNormFlags() const{return fNormFlags;}

  void SetCutExpr(Option_t *cutexpr=NULL){fCutExpr=cutexpr;}
  enum eNormFlags {kRegularNorm=0, kOneFixedCoord=1, kTwoFixedCoords=2, kThreeFixedCoords=3, kVarBinSizeEventsFilled=4, kNoBinWidthNorm=56, kNoXBinWidthNorm=8, kNoYBinWidthNorm=16, kNoZBinWidthNorm=32, kNoNorm=64};
  void SetNormFlags(eNormFlags normflags=kRegularNorm)
  {
    //Sets the normalization flags for the QDis object. The available normalization flags are the following:
    //
    //kRegularNorm: Regular normalization (area/volume/hypervolume = 1) (default)
    //kOneFixedCoord: Conditional PDF with 1 fixed coordinate
    //kTwoFixedCoords: Conditional PDF with 2 fixed coordinates
    //kThreeFixedCoords: Conditional PDF with 3 fixed coordinates
    //kVarBinSizeEventsFilled: PDF with variable bin size for which the histogram bin content corresponds to
    //                         a number of events
    //kNoBinWidthNorm: Do not use bin widths at all for PDF normalization
    //kNoXBinWidthNorm: Do not use bin widths in x direction for PDF normalization
    //kNoYBinWidthNorm: Do not use bin widths in y direction for PDF normalization
    //kNoZBinWidthNorm: Do not use bin widths in z direction for PDF normalization
    //kNoNorm: No normalization
    //Flags can be combined using a bitwise "OR" operator (&).
    fNormFlags=normflags;
  }

 protected:
  void SetNameTitleToObject();
  TString fCutExpr;
  eNormFlags fNormFlags;

 private:
  QDis(const QDis& rhs): TNamed(rhs),QTObjectIO(rhs.GetClassName()), QProcObj(rhs){*this=rhs;}

  ClassDef(QDis,1) //Abstract class of QDis* classes
};

#include "debugger.h"
#endif
