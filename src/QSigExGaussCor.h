// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXGAUSSCOR_
#define _QSIGEXGAUSSCOR_

#include "TCollection.h"
#include "TF1.h"
#include "TH1.h"
#include "TTree.h"
#include "TMatrixDSym.h"
#include "QSigExDirHandler.h"
#include "QList.h"
#include "QDisTH.h"
#include "QSigExGaussMapping.h"
#include "QTTreeUtils.h"
#include "QProgress.h"
#include "QNamedVar.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExGaussCor                                                     //
//                                                                    //
// This class, used with class QSigExGCJointProbs, allows to          //
// compute joint probability densities of flux groups considering the //
// correlations between their variables. To achieve this, it creates  //
// for each variable x a mapping function y(x) that has a gaussian    //
// distribution, such that the distribution of y(x) variables for     //
// each flux group is multi-gaussian. The covariance matrix of y(x)   //
// variables being computed for each flux group, it is possible to    //
// compute the joint probabilities using the multi-gaussian equation. //
//                                                                    //
// The class QSigExGaussCor loops over the flux groups in "PDFs"      //
// QSigExStruct and finds the ones that have a clean TTree named      //
// "Event Info". For each flux group, it finds QDisTH PDFs and        //
// produces a y(x) mapping function with gaussian distribution (TF1   //
// object). Then, it uses the set of y(x) functions and the clean     //
// TTree belonging to the flux group to compute the covariance matrix //
// (TMatrixD object).  Matrix indexes are identified using QNamedVar  //
// objects in the PDFs directories.                                   //
//                                                                    //
////////////////////////////////////////////////////////////////////////

class QSigExGaussCor: public QSigExDirHandler{
 public:
  QSigExGaussCor():QSigExDirHandler(){

    PRINTF2(this,"\tQSigExGaussCor::QSigExGaussCor()\n")
  }

  QSigExGaussCor(QSigExStruct* dir,const Char_t* cardfilename=NULL):QSigExDirHandler()
    {
      PRINTF6(this,"\tQSigExGaussCor::QSigExGaussCor(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExGaussCor::QSigExGaussCor\n";
	throw e;
      }
    }

  virtual ~QSigExGaussCor(){}

  void LoadCardFile(const Char_t* cardfilename=NULL);
  Int_t Get();

  void CleanDir();
  void ClearCardBuf(){}

 protected:
  void FormatDir();
  void CheckPDFs() const;

 private:
  ClassDef (QSigExGaussCor,1) //Computes y(x) variables with gaussian distribution and their covariance matrices
};


#include "debugger.h"
#endif





