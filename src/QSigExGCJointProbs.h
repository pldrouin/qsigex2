// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXGCJOINTPROBS_
#define _QSIGEXGCJOINTPROBS_

#include "TMath.h"
#include "TTree.h"
#include "TBranch.h"
#include "TObjArray.h"
#include "TCollection.h"
#include "TMatrixDSym.h"
#include "TDecompChol.h"
#include "TF1.h"
#include "QSigExDirHandler.h"
#include "QTTreeUtils.h"
#include "arrayutils.h"
#include "QProgress.h"
#include "QNamedVar.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExGCJointProbs                                                 //
//                                                                    //
// This class, used with class QSigExGaussCor, allows to compute      //
// joint probability densities of flux groups considering the         //
// correlations between their variables. To achieve this, it can use, //
// for each variable x, a mapping function y(x) that has a gaussian   //
// distribution, such that the distribution of y(x) variables for     //
// each flux group is multi-gaussian. The covariance matrix of y(x)   //
// variables being computed for each flux group, it is possible to    //
// compute the joint probabilities using the multi-gaussian equation. //
//                                                                    //
// The class QSigExGCJointProbs computes the joint probability        //
// densities using already existing covariance matrices and y(x)      //
// mapping functions, as described in QSigExGCJointProbs::Get(). The  //
// output information is stored in a TTree located in                 //
// "Probs/JointPDFsProbs".                                            //
//                                                                    //
////////////////////////////////////////////////////////////////////////

class QSigExGCJointProbs: public QSigExDirHandler{
 public:
  QSigExGCJointProbs(): QSigExDirHandler(),fProbsDir(NULL), fGCJointProbsDir(NULL) {

    PRINTF2(this,"\tQSigExGCJointProbs::QSigExGCJointProbs()\n")
  }

  QSigExGCJointProbs(const QSigExGCJointProbs& rhs):QSigExDirHandler(*this),fProbsDir(rhs.fProbsDir),fGCJointProbsDir(rhs.fGCJointProbsDir){
    PRINTF2(this,"\tQSigExGCJointProbs::QSigExGCJointProbs(const QSigExGCJointProbs& rhs)\n")
  }

  QSigExGCJointProbs(QSigExStruct* dir,const Char_t* cardfilename=NULL): QSigExDirHandler(),fProbsDir(NULL), fGCJointProbsDir(NULL)
    {
      PRINTF6(this,"\tQSigExGCJointProbs::QSigExGCJointProbs(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExGCJointProbs::QSigExGCJointProbs\n";
	throw e;
      }
    }

  virtual ~QSigExGCJointProbs(){}

  void LoadCardFile(const Char_t* cardfilename=NULL);
  Int_t Get();

  void CleanDir();
  void ClearCardBuf(){}

 protected:
  void FormatDir();
  void CheckPDFs() const;
  void CheckPDFsProbs() const;
  void CheckCleanData() const;

 private:
  TDirectory* fProbsDir; //Pointer to the TDirectory that contains the probabilities
  TDirectory* fGCJointProbsDir; //Pointer to the TDirectory that contains the joint probability densities from the PDFs using gaussian correlations

  ClassDef (QSigExGCJointProbs,1) //Computes joint probability densities considering correlations between variables
};


#include "debugger.h"
#endif





