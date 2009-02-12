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
#include "QProcObj.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QDis                                                               //
//                                                                    //
// This class is an abstract base class which allows to give a common //
// interface to all types of ROOT "functions" (TH1, TH2, TH3, TF1,    //
// TF2, TF3) such that they can be transparently used as probability  //
// density functions. QDis declares a Normalize function that allows  //
// to normalize the functions and a ProbDensity function that returns //
// the probability density associated with a given point.             //
//                                                                    //
////////////////////////////////////////////////////////////////////////

class QDis: public TNamed, public QProcObj
{
 public:
  QDis(): fCutExpr(), fNormFlags(kRegularNorm), fNFixedCoords(0)
    {
      PRINTF2(this,"\tQDis::QDis()\n")
    }
  
  QDis(const QDis& rhs): TNamed(rhs), QProcObj(rhs){*this=rhs;}

  virtual ~QDis();

  virtual Double_t ProbDensity(const Double_t &x,const Double_t &y=0,const Double_t &z=0) const=0;
  virtual Double_t Derivative(const Double_t&) const {return 0;};
  

  virtual QDis* CloneQDis() const=0;

  virtual void Normalize(Double_t* integral=NULL)=0;

  const QDis& operator=(const QDis &rhs){
    TNamed::operator=(rhs);
    fCutExpr=rhs.fCutExpr;
    fNormFlags=rhs.fNormFlags;
    fNFixedCoords=rhs.fNFixedCoords;
    return *this;
  }

  virtual void Draw(Option_t *option="")=0;

  virtual void Browse(TBrowser* b)
    {
      b=NULL;
      Draw();
      gPad->Update();
    }

  virtual Int_t GetDimension() const=0;

  const Option_t* GetCutExpr() const{return fCutExpr;}
  UInt_t GetNFixedCoords() const{return fNFixedCoords;}
  UInt_t GetNormFlags() const{return fNormFlags;}

  void SetCutExpr(Option_t *cutexpr=NULL){fCutExpr=cutexpr;}
  void SetNFixedCoords(UInt_t nfixedcoords=0){
    //The argument specifies the number of fixed coordinates (i.e. number of conditional variables for a
    //conditional PDF). Fixed coordinates must have the highest axis indices.
    fNFixedCoords=nfixedcoords;
  }
  void SetNormFlags(UInt_t normflags=kRegularNorm)
  {
    //Sets the normalization flags for the QDis object. The available normalization flags are the following:
    //
    //kRegularNorm: Regular normalization (area/volume/hypervolume = 1) (default)
    //kEventsFilled: Faster normalization algorithm for histograms which bin content corresponds to a number of
    //               of events. Underflow and overflow bins must be empty. Should not normalize more than once
    //               using this option. kVarBinSizeEventsFilled is automatically added for histograms having a
    //               variable bin size. Has no effect for a conditional PDF.
    //kVarBinSizeEventsFilled: PDF with variable bin size for which the histogram bin content corresponds to
    //                         a number of events (should not normalize more than once using this option).
    //                         Has no effect for histograms having constant bin width in all directions or if
    //                         using kNoBinWidthNorm.
    //kNoBinWidthNorm: Do not use bin widths for PDF normalization
    //kNoNorm: No normalization
    //Flags can be combined using a bitwise "OR" operator (|).
    fNormFlags=normflags;
  }
  enum eNormFlags {kRegularNorm=0, kEventsFilled=1, kVarBinSizeEventsFilled=2, kNoBinWidthNorm=4, kNoNorm=8};

 protected:
  virtual void SetNameTitleToObject()=0;
  TString fCutExpr;
  UInt_t fNormFlags;
  UInt_t fNFixedCoords;

 private:
  ClassDef(QDis,2) //Abstract class of QDis* classes
};

#include "debugger.h"
#endif
